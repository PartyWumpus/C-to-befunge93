#!/usr/bin/env python3

import json, subprocess, sys, re, os, time

compiler = "./target/release/c-to-befunge"
# TODO: fix the path
befunge_interpeter = "./RBeJ/target/release/rbej"
befunge_out_path = "out.b93"

compile_fails = []
incorrect_execution = []
interpreter_crashes = []
successes = []

accepted_invalid_code = []

valid_tests = {}
invalid_tests = []

chapter_regex = ".*chapter_(1|2|3|4|5|6|7|8|9|10)/.*"

# find valid tests
with open(f"./writing-a-c-compiler-tests/expected_results.json") as f:
    data = json.loads(f.read())
    for test in data:
        if re.match(chapter_regex, test):
            valid_tests[f"./writing-a-c-compiler-tests/tests/{test}"] = data[test]["return_code"]

for root, dirs, files in os.walk("./writing-a-c-compiler-tests/tests/"):
    if re.match(chapter_regex, root) and "/valid" not in root:
        for file in files:
            invalid_tests.append(root + "/" + file)

valid_tests = dict(sorted(valid_tests.items()))
invalid_tests.sort()

if len(sys.argv) > 1:
    print(valid_tests[sys.argv[1]])
    exit(0)

# compile compiler
subprocess.run(["cargo", "build", "--release"])

# run tests
print("\033[0;35m\nINVALID C TESTS")
time.sleep(1)

for test in invalid_tests:
    print(f"\033[0m{test}", end="");
    sys.stdout.flush();
    res = subprocess.run([compiler, test, "-s", "-o", befunge_out_path])
    if res.returncode != 0: # compiler failed
        print(f"\r\033[1;32mPASS {test}")
        successes.append(test)
    else:
        print(f"\r\033[1;33mINVALID ACCEPTED {test}")
        accepted_invalid_code.append(test)

print("\033[0;35m\n\nVALID C TESTS")
time.sleep(0.5)
for test in valid_tests:
    print(f"\033[0m{test}", end="");
    sys.stdout.flush();
    res = subprocess.run([compiler, test, "-q", "-o", befunge_out_path])
    if res.returncode != 0: # compiler failed
        compile_fails.append(test)
        print(f"\r\033[1;33mFAIL COMPILATION {test}")
        continue

    try:
        res = subprocess.run([befunge_interpeter, befunge_out_path],capture_output=True,text=True,timeout=3)
    except subprocess.TimeoutExpired:
        interpreter_crashes.append(test)
        print(f"\r\033[1;33mINTERPRETER TIMEOUT {test}")
        continue
    if res.returncode != 0: # interpeter crashed
        interpreter_crashes.append(test)
        print(f"\r\033[1;33mINTERPRETER CRASH {test}")
        continue

    if (int(res.stdout.splitlines()[-1]) % 256 != valid_tests[test]):
        incorrect_execution.append(test)
        print(f"\r\033[1;31mFAIL EXECUTION (got: {res.stdout} expected: {valid_tests[test]}) {test}")
    else:
        print(f"\r\033[1;32mPASS {test}")
        successes.append(test)


print("\033[0m")

print(f"compile failures: {compile_fails}\n")
print(f"incorrect execution: {incorrect_execution}\n")
print(f"interpreter crashes: {interpreter_crashes}\n")
print(f"invalid code acceptances: {accepted_invalid_code}\n")

total_tests = len(successes) + len(interpreter_crashes) + len(compile_fails) + len(accepted_invalid_code) + len(incorrect_execution)

print(f"\033[1;33minterpreter crashes: {len(interpreter_crashes)}\033[0m")
print(f"\033[1;33mcompile fails: {len(compile_fails)}\033[0m")
print(f"\033[1;33minvalid acceptances: {len(accepted_invalid_code)}\033[0m")
print(f"\033[1;31mincorrect execution: {len(incorrect_execution)}\033[0m")
print(f"\033[1;32msuccesses: {len(successes)}\033[0m / {total_tests}")
