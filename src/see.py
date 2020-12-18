#!/usr/bin/env python3

import argparse
import subprocess as sp

import sexpdata as sexp


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
    out = get_stdout(res)
    program_name, *formulas = out.split("\n;SEP\n")
    
    if args.dry_run: # dry run
      print(formulas)
    
    else:
      models = list()
      for formula in formulas:
        cmd = f"echo \"{formula}\" | z3 -in"
        res = sp.run(cmd, shell=True, capture_output=True, check=True)
        out = [l.strip() for l in get_stdout(res).split('\n')]

        if "sat" in out:
          model_str = "".join(out[1:])
          model = [pair[1] for pair in sexp.loads(model_str)]
          models.append(model)
        elif "unsat" in out:
          pass
        else:
          raise ValueError("Output is neither sat nor unsat")
      
      if len(models) == 0:
        print(VALID_MSG)

      else:
        print(INVALID_MSG)
        for model in models:
          print(program_name.strip(), " ".join(map(str, model)))

  except sp.CalledProcessError as err:
    print(ERROR_MSG, get_stderr(err))
    print(get_stdout(err))
    

