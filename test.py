#!/usr/bin/env python3

from enum import IntEnum
from dataclasses import dataclass
import json, subprocess, sys, re, os, asyncio, tempfile, argparse
from collections.abc import Awaitable, Sequence

compiler = "./target/release/c-to-befunge"
befunge_interpeter = "./RBeJ/target/release/rbej"
befunge_out_path = "out.b93"

NUM_CORES = 7

parser = argparse.ArgumentParser(prog='c-to-befunge tester')
subparsers = parser.add_subparsers(
    title="subcommands", dest="command"
)
run_parser = subparsers.add_parser("run", help="Run a single test")
run_parser.add_argument("filename")

suite_parser = subparsers.add_parser("test", help="Run full test suite")
suite_parser.add_argument("--quiet", "-q", action='store_true')
suite_parser.add_argument("--silent", "-s", action='store_true')
suite_parser.add_argument("--save-diff", action='store_true')
suite_parser.add_argument("--check-diff", action='store_true')
suite_parser.add_argument("--extra-credit", action='store_true')
suite_parser.add_argument("--chapters", nargs='+', default=['1','2','3','4','5','6','7','8','9','10'])

args = parser.parse_args()

def find_valid_tests(chapter_regex: str):
    valid_tests: dict[str, str] = {}
    # find valid tests
    with open(f"./writing-a-c-compiler-tests/expected_results.json") as f:
        data = json.loads(f.read())
        for test in data:
            if re.match(chapter_regex, test):
                valid_tests[f"./writing-a-c-compiler-tests/tests/{test}"] = data[test]["return_code"]
    
    valid_tests = dict(sorted(valid_tests.items()))
    return valid_tests

def find_invalid_tests(chapter_regex: str):
    invalid_tests: list[str] = []
    for root, _dirs, files in os.walk("./writing-a-c-compiler-tests/tests/"):
        if re.match(chapter_regex, root) and "/valid" not in root:
            for file in files:
                invalid_tests.append(root + "/" + file)

    invalid_tests.sort()
    return invalid_tests

class TestType(IntEnum):
    Invalid = 0
    Valid = 1

class ResultType(IntEnum):
    COMPILE_FAIL = 0
    INCORRECT_EXECUTION = 1
    INTERPRETER_TIMEOUT = 2
    INTERPRETER_CRASH = 3
    INVALID_ACCEPTED = 4
    SUCCESS = 5

class Status(IntEnum):
    RED = 0
    YELLOW = 1
    GREEN = 2

@dataclass
class Result:
    info: str
    result: ResultType
    status: Status
    stdout: bytes

def status_to_code(status: Status):
    match status:
        case Status.RED:
            return "\033[1;31m"
        case Status.YELLOW:
            return "\033[0;33m"
        case Status.GREEN:
            return "\033[1;32m"

# run tests
async def test_invalid(test: str) -> Result:
    with tempfile.NamedTemporaryFile() as tf:
        proc = await asyncio.create_subprocess_shell(f"{compiler} {test} -q -o {tf.name}", stdout=asyncio.subprocess.PIPE,
        stderr=asyncio.subprocess.STDOUT)
        stdout, _ = await proc.communicate()

        if proc.returncode != 0: # compiler failed, which is what we want
            return Result(f"PASS {test}", ResultType.SUCCESS, Status.GREEN, stdout)
        else:
            return Result(f"INVALID ACCEPTED {test}", ResultType.INVALID_ACCEPTED, Status.YELLOW, stdout)

async def test_valid(test: str, expected: str | None) -> Result:
    with tempfile.NamedTemporaryFile() as tf:
        proc = await asyncio.create_subprocess_shell(f"{compiler} {test} -q -o {tf.name}", stdout=asyncio.subprocess.PIPE,
        stderr=asyncio.subprocess.STDOUT)
        stdout, _ = await proc.communicate()
        if proc.returncode != 0: # compiler failed
            return Result(f"FAIL COMPILATION {test}", ResultType.COMPILE_FAIL, Status.RED, stdout)

        proc = await asyncio.create_subprocess_shell(f"{befunge_interpeter} {tf.name}",stdout=asyncio.subprocess.PIPE,
        stderr=asyncio.subprocess.STDOUT)
        try:
            stdout, _ = await asyncio.wait_for(proc.communicate(), 5)
        except asyncio.TimeoutError:
            proc.kill()
            return Result(f"INTERPRETER TIMEOUT {test}", ResultType.INTERPRETER_TIMEOUT, Status.YELLOW, bytes())

        if proc.returncode != 0: # interpreter crashed
            return Result(f"INTERPRETER CRASH {test}", ResultType.INTERPRETER_CRASH, Status.YELLOW, stdout)

        if expected is not None and int(stdout.splitlines()[-1]) % 256 != expected:
            return Result(f"FAIL EXECUTION (got: {stdout.splitlines()[-1]} expected: {expected}) {test}", ResultType.INCORRECT_EXECUTION, Status.RED, stdout)

        return Result(f"PASS {test}", ResultType.SUCCESS, Status.GREEN, stdout)

def move_cursor_up(n: int):
    print("\033[F"*n)

in_progress_tests = ["... waiting"]*NUM_CORES
async def test_runner(
        tsts: Sequence[tuple[TestType, tuple[str, str | None]]], 
        scores: list[int], 
        counts: dict[ResultType, list[str]], 
        n:int
    ) -> None:
    if n == 0 and not args.silent:
          print("\n"*NUM_CORES)
    while True:
        if len(tsts) > 0:
            test = tsts.pop()
        else:
            break
        in_progress_tests[n] = test[1][0]
        if test[0] == TestType.Valid:
            res = await test_valid(test[1][0], test[1][1])
        else:
            res = await test_invalid(test[1][0])
        if not args.silent:
            move_cursor_up(NUM_CORES+2)
        scores[res.status] += 1
        counts[res.result].append(test[1][0])
        if not args.quiet and not args.silent:
            if res.status != Status.GREEN:
                sys.stdout.write(status_to_code(res.status))
                sys.stdout.write(res.info)
                sys.stdout.write("\033[0m") # reset
                sys.stdout.write("\n")
                if res.stdout != b'':
                    sys.stdout.write("\33[2K\r")
                    sys.stdout.write(res.stdout.decode("utf-8").strip().replace("\n", "\n\33[2K\r"))
                    sys.stdout.write("\n")
            sys.stdout.write("\33[2K\r")
            sys.stdout.write(f"{scores[0] + scores[1] + scores[2]}/{len(valid_tests) + len(invalid_tests)}")

        if not args.silent:
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

async def run_tests(valid_tests: dict[str, str], invalid_tests: list[str]):
    tasks: Sequence[Awaitable[None]] = []
    tests: Sequence[tuple[TestType, tuple[str, str | None]]] = list(map(lambda x: (TestType.Valid, x), valid_tests.items())) + list(map(lambda x: (TestType.Invalid, (x, None)), invalid_tests))
    scores = [0,0,0]
    counts = {s: [] for s in ResultType}
    for i in range(NUM_CORES):
        tasks.append(test_runner(tests, scores, counts, i))
    res = await asyncio.gather(*tasks, return_exceptions=True)
    if not args.silent:
        sys.stdout.write("\n")
    for i, err in enumerate(res):
        if err != None:
            print(err)
    sys.stdout.flush()
    return counts

async def run_single_test(test: str):
    with open(f"./writing-a-c-compiler-tests/expected_results.json") as f:
        valid_tests = json.loads(f.read())
    if test.removeprefix("./writing-a-c-compiler-tests/tests/") in valid_tests:
        res = await test_valid(test, valid_tests[test.removeprefix("./writing-a-c-compiler-tests/tests/")])
        print(res.info)
        print("stdout:", res.stdout.decode())

    else:
        print(f"{test} not a known test, running anyways")
        res = await test_valid(test, None)
        print(res.info)
        print("stdout:", res.stdout.decode())


subprocess.run(["cargo", "build", "--release"], capture_output=True)
if args.command == "run":
    asyncio.run(run_single_test(args.filename))
elif args.command == "test":
    if args.extra_credit:
        chapter_regex = f"^.*chapter_({'|'.join(args.chapters)})/.*$"
    else:
        chapter_regex = f"^.*chapter_({'|'.join(args.chapters)})/((?!extra_credit/).)*$"

    valid_tests, invalid_tests = find_valid_tests(chapter_regex), find_invalid_tests(chapter_regex)
    counts = asyncio.run(run_tests(valid_tests, invalid_tests))

    if args.check_diff:
        with open("test_diff.json", "r") as f:
            data = json.load(f)
    if args.save_diff:
        with open("test_diff.json", "w") as f:
            json.dump(counts, f)

    def wawa(text: str, *restypes: ResultType):
        a = sum([counts[restype] for restype in restypes], [])
        if args.check_diff:
            b = sum([data[str(restype)] for restype in restypes], [])
            gains = len(list(set(a) - set(b)))
            losses = len(list(set(b) - set(a)))
            print(f"{text}: {len(a)}, +{gains} -{losses}\033[0m")
            if not args.quiet:
                if len(list(set(a) - set(b))) > 0:
                    print("+ ", list(set(a) - set(b)))
                if len(list(set(b) - set(a))) > 0:
                    print("- ", list(set(b) - set(a)))
        else:
            print(f"{text}: {len(a)}\033[0m")

    wawa(f"\033[1;33minvalid acceptances: ", ResultType.INVALID_ACCEPTED)
    wawa(f"\033[1;33minterpreter crashes: ", ResultType.INTERPRETER_CRASH, ResultType.INTERPRETER_TIMEOUT)
    wawa(f"\033[1;33mcompile fails: ", ResultType.COMPILE_FAIL)
    wawa(f"\033[1;31mincorrect execution: ", ResultType.INCORRECT_EXECUTION)
    wawa(f"\033[1;32msuccesses: ", ResultType.SUCCESS)
