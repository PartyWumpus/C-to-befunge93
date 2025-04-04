#!/usr/bin/env python3

from enum import IntEnum
import json, subprocess, sys, re, os, asyncio, tempfile
# TODO: consider returning the tracebacks n stuff for each test

compiler = "./target/release/c-to-befunge"
befunge_interpeter = "./RBeJ/target/release/rbej"
befunge_out_path = "out.b93"

compile_fails = []
incorrect_execution = []
interpreter_crashes = []
successes = []

accepted_invalid_code = []

valid_tests: dict[str, str] = {}
invalid_tests: list[str] = []

scores = [0,0,0]

NUM_CORES = 7

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
subprocess.run(["cargo", "build", "--release"], capture_output=True)

class TestType(IntEnum):
    Invalid = 0
    Valid = 1

class Status(IntEnum):
    RED = 0
    YELLOW = 1
    GREEN = 2

def status_to_code(status: Status):
    match status:
        case Status.RED:
            return "\033[1;31m"
        case Status.YELLOW:
            return "\033[0;33m"
        case Status.GREEN:
            return "\033[1;32m"

# run tests
async def test_invalid(test: str) -> tuple[str, Status]:
    with tempfile.NamedTemporaryFile() as tf:
        proc = await asyncio.create_subprocess_shell(f"{compiler} {test} -q -o {tf.name}", stdout=asyncio.subprocess.PIPE,
        stderr=asyncio.subprocess.PIPE)
        await proc.communicate()
        if proc.returncode != 0: # compiler failed, which is what we want
            successes.append(test)
            return (f"PASS {test}", Status.GREEN)
        else:
            accepted_invalid_code.append(test)
            return (f"INVALID ACCEPTED {test}", Status.YELLOW)

async def test_valid(test: str) -> tuple[str, Status]:
    with tempfile.NamedTemporaryFile() as tf:
        proc = await asyncio.create_subprocess_shell(f"{compiler} {test} -q -o {tf.name}", stdout=asyncio.subprocess.PIPE,
        stderr=asyncio.subprocess.PIPE)
        await proc.communicate()
        if proc.returncode != 0: # compiler failed
            compile_fails.append(test)
            return (f"FAIL COMPILATION {test}", Status.RED)

        proc = await asyncio.create_subprocess_shell(f"{befunge_interpeter} {tf.name}",stdout=asyncio.subprocess.PIPE,
        stderr=asyncio.subprocess.PIPE)
        try:

            stdout, stderr = await asyncio.wait_for(proc.communicate(), 5)
        except asyncio.TimeoutError:
            proc.kill()
            interpreter_crashes.append(test)
            return (f"INTERPRETER TIMEOUT {test}", Status.YELLOW)

        if proc.returncode != 0: # interpeter crashed
            interpreter_crashes.append(test)
            return (f"INTERPRETER CRASH {test}", Status.YELLOW)

        if (int(stdout.splitlines()[-1]) % 256 != valid_tests[test]):
            incorrect_execution.append(test)
            return (f"FAIL EXECUTION (got: {stdout.splitlines()[-1]} expected: {valid_tests[test]}) {test}", Status.RED)

        successes.append(test)
        return (f"PASS {test}", Status.GREEN)

def move_cursor_up(n: int):
    print("\033[F"*n)

in_progress_tests = ["... waiting"]*NUM_CORES
async def test_runner(tsts: list[tuple[TestType, str]], n:int):
    if n == 0:
          print("\n"*NUM_CORES)
    while True:
        if len(tsts) > 0:
            test = tsts.pop()
        else:
            break
        in_progress_tests[n] = test[1]
        if test[0] == TestType.Valid:
            res = await test_valid(test[1])
        else:
            res = await test_invalid(test[1])
        move_cursor_up(NUM_CORES+2)
        scores[res[1]] += 1
        if res[1] != Status.GREEN:
            sys.stdout.write(status_to_code(res[1]))
            sys.stdout.write(res[0])
            sys.stdout.write("\033[0m") # reset
            sys.stdout.write("\n")
        sys.stdout.write("\33[2K\r")
        sys.stdout.write(f"{scores[0] + scores[1] + scores[2]}/{len(valid_tests) + len(invalid_tests)}")

        for i in Status:
            sys.stdout.write(status_to_code(i))
            sys.stdout.write(f" {scores[i]}")
        sys.stdout.write("\033[0m") # reset
        sys.stdout.write("\n")
        for in_prog in in_progress_tests:
            sys.stdout.write("\33[2K\r")
            sys.stdout.write(f"{in_prog}\n")
        sys.stdout.flush()
    in_progress_tests[n] = "done"

async def run_tests():
    tasks = []
    tests: list[tuple[TestType, str]] = list(map(lambda x: (TestType.Valid, x), valid_tests)) + list(map(lambda x: (TestType.Invalid, x), invalid_tests))
    for i in range(NUM_CORES):
        tasks.append(test_runner(tests, i))
    res = await asyncio.gather(*tasks, return_exceptions=True)
    sys.stdout.write("\n")
    for i, err in enumerate(res):
        if err != None:
            print(err)
    sys.stdout.flush()


asyncio.run(run_tests())
print("\033[0m")

total_tests = len(successes) + len(interpreter_crashes) + len(compile_fails) + len(accepted_invalid_code) + len(incorrect_execution)


print(f"\033[1;33minvalid acceptances: {len(accepted_invalid_code)}\033[0m")
print(f"\033[1;33minterpreter crashes: {len(interpreter_crashes)}\033[0m")
print(f"\033[1;33mcompile fails: {len(compile_fails)}\033[0m")
print(f"\033[1;31mincorrect execution: {len(incorrect_execution)}\033[0m")
print(f"\033[1;32msuccesses: {len(successes)}\033[0m / {total_tests}")
