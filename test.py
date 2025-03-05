#!/usr/bin/env python3

import json, subprocess, sys, re

compiler = "./target/release/c-to-befunge"
# TODO: fix the path
befunge_interpeter = "./RBeJ/target/release/rbej"
befunge_out_path = "out.b93"

compile_fails = []
incorrect_execution = []
interpreter_crashes = []
successes = []

tests = {}

# run tests
with open(f"./writing-a-c-compiler-tests/expected_results.json") as f:
    data = json.loads(f.read())
    for test in data:
        if re.match("chapter_(1|2|3|4|5|6|7|8|9)/", test):
            tests[f"./writing-a-c-compiler-tests/tests/{test}"] = data[test]["return_code"]

tests = dict(sorted(tests.items()))

if len(sys.argv) > 1:
    print(tests[sys.argv[1]])
    exit(0)

# compile compiler
subprocess.run(["cargo", "build", "--release"])

for test in tests:
    print(f"\033[0m{test}", end="");
    sys.stdout.flush();
    res = subprocess.run([compiler, test, "-s", "-o", befunge_out_path])
    if res.returncode != 0: # compiler failed
        compile_fails.append(test)
        print(f"\r\033[1;33mFAIL COMPILATION {test}")
        continue

    try:
        res = subprocess.run([befunge_interpeter, befunge_out_path],capture_output=True,text=True,timeout=5)
    except subprocess.TimeoutExpired:
        interpreter_crashes.append(test)
        print(f"\r\033[1;33mINTERPRETER TIMEOUT {test}")
        continue
    if res.returncode != 0: # interpeter crashed
        interpreter_crashes.append(test)
        print(f"\r\033[1;33mINTERPRETER CRASH {test}")
        continue

    if (int(res.stdout) % 256 != tests[test]):
        incorrect_execution.append(test)
        print(f"\r\033[1;31mFAIL EXECUTION (got: {res.stdout} expected: {tests[test]}) {test}")
    else:
        print(f"\r\033[1;32mPASS {test}")
        successes.append(test)

print("\033[0m")

print(f"compile_fails: {compile_fails}\n")
print(f"incorrect_execution: {incorrect_execution}\n")
print(f"interpreter_crashes: {interpreter_crashes}\n")

print(f"\033[1;33minterpreter crashes: {len(interpreter_crashes)}\033[0m")
print(f"\033[1;33mcompile fails: {len(compile_fails)}\033[0m")
print(f"\033[1;31mincorrect execution: {len(incorrect_execution)}\033[0m")
print(f"\033[1;32msuccesses: {len(successes)}\033[0m")
