import argparse
import glob
import os
import re
import subprocess
from subprocess import PIPE, Popen
import sys

TEST_DIR = "test"

EXPECT_OUTPUT_RE = re.compile("// expect: ?(.*)")
EXPECT_RUNTIME_ERROR_RE = re.compile("// expect runtime error: ?(.*)")
EXPECT_COMPILE_ERROR_RE = re.compile("// expect compile error: ?(.*)")


class TestError(Exception):
    def __init__(self, message):
        self.message = message


def get_args():
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--oba", help="The path to the oba interpreter", default="./oba"
    )
    parser.add_argument("--glob", help="Filter test files by name", default="**/*.oba")
    return parser.parse_args()


def verify_expectations(expectations, output_lines, output_name):
    errors = []
    line = 0
    while line < len(expectations):
        want = expectations[line]
        got = None
        if line < len(output_lines):
            got = output_lines[line]
        if want != got:
            errors.append(
                "Got {} but wanted {} at {} line {}".format(
                    repr(got), repr(want), output_name, line
                )
            )
        line += 1

    if line < len(output_lines):
        extra_lines = "\n".join(output_lines[line:])
        errors.append("Extra lines in output:\n{}\n".format(extra_lines))

    return errors


def run_test(oba, test_file):
    # Get the test output.
    test_args = [oba, test_file]
    proc = Popen(test_args, stdin=PIPE, stderr=PIPE, stdout=PIPE)
    stdout, stderr = proc.communicate()

    try:
        stdout = stdout.decode("utf-8").replace("\r\n", "\n")
        stderr = stderr.decode("utf-8").replace("\r\n", "\n")
    except:
        return ["failed decoding output"]

    expected_outs = []
    expected_errs = []

    # Parse the test expectations.
    with open(test_file, "r") as f:
        for line in f.readlines():
            match = EXPECT_OUTPUT_RE.search(line)
            if match:
                expected_outs.append(match.group(1))

            match = EXPECT_RUNTIME_ERROR_RE.search(line)
            if match:
                expected_errs.append("Runtime error: " + match.group(1))

            match = EXPECT_COMPILE_ERROR_RE.search(line)
            if match:
                expected_errs.append("Compile error: " + match.group(1))

    if expected_outs and expected_errs:
        raise TestError("Cannot expect both errors and output in the same test")
    if not expected_outs and not expected_errs:
        raise TestError("Test has no expectations")

    if expected_outs:
        return verify_expectations(expected_outs, stdout.splitlines(), "stdout")
    return verify_expectations(expected_errs, stderr.splitlines(), "stderr")


def run_test_file(oba, test_file):
    test_name = os.path.relpath(test_file, TEST_DIR)

    try:
        errors = run_test(oba, test_file)
    except TestError as e:
        print("- FATAL ERROR: ", test_name)
        print("  {}".format(e))
        return

    if not errors:
        print("- PASS " + test_name)
        return

    print("- FAIL " + test_name)
    for error in errors:
        print("  - ERROR: " + error)


def run_test_files(oba, filepaths):
    for filepath in filepaths:
        run_test_file(oba, filepath)


def main():
    args = get_args()

    test_file_glob = os.path.join(TEST_DIR, args.glob)
    test_files = glob.glob(test_file_glob, recursive=True)
    run_test_files(args.oba, test_files)


main()
