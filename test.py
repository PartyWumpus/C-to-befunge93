#!/usr/bin/env python3

import json, subprocess, sys

tests_path = "./writing-a-c-compiler-tests"
compiler = "./target/release/c-to-befunge"
# TODO: fix the path
befunge_interpeter = "../befunging/target/release/rbej"
befunge_out_path = "out.b93"

# compile
subprocess.run(["cargo", "build", "--release"])

compile_fails = []
incorrect_execution = []
interpreter_crashes = []
successes = []

# run tests
with open(f"{tests_path}/expected_results.json") as f:
    data = json.loads(f.read())
    for file in data:
        print(f"\033[0m{file}", end="");
        sys.stdout.flush();
        res = subprocess.run([compiler, f"{tests_path}/tests/{file}", "-s", "-o", befunge_out_path])
        if res.returncode != 0: # compiler failed
            compile_fails.append(file)
            print(f"\r\033[0;31mFAIL COMPILATION {file}")
            continue

        res = subprocess.run([befunge_interpeter, befunge_out_path],capture_output=True,text=True)
        if res.returncode != 0: # interpeter crashed??
            interpreter_crashes.append(file)
            print(f"\r\033[1;33mINTERPRETER CRASH {file}")
            continue

        if (int(res.stdout) != data[file]["return_code"]):
            incorrect_execution.append(file)
            print(f"\r\033[0;31mFAIL EXECUTION (got: {res.stdout} expected: {data[file]["return_code"]}) {file}")
        else:
            print(f"\r\033[1;32mPASS {file}")
            successes.append(file)

print(f"compile_fails: {compile_fails}")
print(f"incorrect_execution: {incorrect_execution}")
print(f"interpreter_crashes: {interpreter_crashes}")

print(f"compile_fails: {len(compile_fails)}")
print(f"incorrect_execution: {len(incorrect_execution)}")
print(f"interpreter_crashes: {len(interpreter_crashes)}")
print(f"successes: {len(successes)}")
