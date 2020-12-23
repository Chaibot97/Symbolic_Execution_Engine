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
ENCODING = "utf-8"


def decode_pair(pair, post=lambda s: s.strip()):
  decode = lambda b: None if b is None else post(b.decode(ENCODING))
  return tuple(map(decode, pair))

def get_output(res):
  return decode_pair((res.stdout, res.stderr))

if __name__ == "__main__":
  args = parser.parse_args()
  
  def verify(formula):
    z3 = sp.Popen(["z3", "-in"], stdin=sp.PIPE, stdout=sp.PIPE)
    fb = formula.encode(ENCODING)
    out, err = z3.communicate(fb)
    return decode_pair((out, err))


  
  try:
    cmd = f"cabal -v0 run see-hs {args.file.name} {args.n}"
    out, err = get_output(sp.run(cmd, shell=True, capture_output=True, check=True))
    program_name, *formulas = out.split("\n\n;SEP\n\n")

    if len(formulas) == 0:
      print(VALID_MSG)
      print("No assertion to check")
    
    else:
      if args.dry_run: # dry run
        for i, formula in enumerate(formulas):
          print(f"; Formula {i}")
          print(formula)
          print()
      
      else:
        models = list()
        for formula in formulas:

          out, err = verify(formula)

          if "sat" == out: # need to query z3 again for a model

            # the last line has the form `;_v1 _v2 ... vn` where 
            # _v1 through _vn are symbolic values for the parameters
            vs = formula.split('\n')[-1][1:].split()
            
            # append a line to get the model
            get_value = "\n(get-value ({}))".format(" ".join(vs))
            formula += get_value
            
            # run the same command with a new formula
            out, err = verify(formula)

            # parse the model as an S-expression
            model_str = "".join([l.strip() for l in out.split('\n')][1:])
            parsed = sexp.loads(model_str)
            model = [pair[1] for pair in parsed] # pair[0] is variable name
            models.append(model)

          elif "unsat" == out:
            pass
          else:
            raise ValueError(f"Output is neither sat nor unsat: {out}")
        
        if len(models) == 0:
          print(VALID_MSG)

        else:
          print(INVALID_MSG)
          for model in models:
            res = []
            for v in model:
              if type(v) == int:
                res.append(str(v))
              else:
                res.append(sexp.dumps(v))
            print(program_name.strip(), " ".join(res))

  except sp.CalledProcessError as e:
    out, err = get_output(e)
    print(ERROR_MSG, err)
    print(out)
    

