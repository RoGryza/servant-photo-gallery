#!/usr/bin/env python
import subprocess
import sys
from glob import glob
from subprocess import Popen

LINTERS = [
    ["weeder", "."],
    ["hlint", "."],
    ["brittany", "--check-mode"] + glob("**/*.hs"),
]

def main():
    procs = [Popen(args, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True) for args in LINTERS]
    success = True
    for proc in procs:
        proc.wait()
    for args, proc in zip(LINTERS, procs):
        if proc.returncode == 0:
            print(f"{args[0]} OK")
            continue
        success = False
        print(f"{args[0]} failed:")
        print(proc.stdout.read())
    if not success:
        sys.exit(1)

if __name__ == '__main__':
    main()
