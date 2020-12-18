#!/usr/bin/env python3

import argparse
import subprocess as sp


parser = argparse.ArgumentParser()
parser.add_argument("file", type=argparse.FileType('r'), help="program source file")
parser.add_argument("n", type=int, help="maximum number of times loops are unrolled")
parser.add_argument("-d", "--dry-run", action="store_true", help="only print the SMT formula")


VALID_MSG = "Verified"
INVALID_MSG = "Not verified"
ERROR_MSG = "Error"


def get_stdout(res):
  return res.stdout.decode('utf-8').strip()

def get_stderr(res):
  return res.stderr.decode('utf-8').strip()


if __name__ == "__main__":
  args = parser.parse_args()
  
  try:
    cmd = f"cabal -v0 run see-hs {args.file.name} {args.n}"
    res = sp.run(cmd, shell=True, capture_output=True, check=True)
    
    if args.dry_run: # dry run
      print(get_stdout(res))
    
    else:
      cmd = f"echo \"{res}\" | z3 -in"
      res = sp.run(cmd, shell=True, capture_output=True, check=True)
      
      if get_stdout(res) == "sat":
        print(VALID_MSG)
      else:
        print(INVALID_MSG)

  except sp.CalledProcessError as err:
    print(ERROR_MSG, get_stderr(err))
    print(get_stdout(err))
    

