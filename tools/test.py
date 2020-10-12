import sys
import argparse
import glob
import subprocess


def get_args():
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--oba", help="The path to the oba interpreter", default="./oba"
    )
    parser.add_argument("--glob", help="Filter test files by name", default="**/*.oba")
    return parser.parse_args()


def run_test_files(oba, files):
    for filename in files:
        subprocess.run([oba, filename])


def main():
    args = get_args()
    test_files = glob.glob("test/" + args.glob, recursive=True)
    run_test_files(args.oba, test_files)


main()
