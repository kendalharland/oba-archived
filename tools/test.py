import argparse
import glob
import os
import re
import subprocess
import sys

TEST_DIR = "test"


def get_args():
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--oba", help="The path to the oba interpreter", default="./oba"
    )
    parser.add_argument("--glob", help="Filter test files by name", default="**/*.oba")
    return parser.parse_args()


def run_test(oba, test_name, test_file):
    proc = subprocess.run(
        [oba, test_file], stderr=subprocess.PIPE, stdout=subprocess.PIPE
    )
    print(proc.stdout)


def run_test_files(oba, filepaths):
    for filepath in filepaths:
        test_name = os.path.relpath(filepath, TEST_DIR)
        run_test(oba, test_name, filepath)


def main():
    args = get_args()

    test_file_glob = os.path.join(TEST_DIR, args.glob)
    test_files = glob.glob(test_file_glob, recursive=True)
    run_test_files(args.oba, test_files)


main()
