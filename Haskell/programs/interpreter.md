/* Represents the linear and (lower) bounded (by 0) memory of the program*/
type memory := 
  { size           : int  /* size of the memory */
  , memory_pointer : int  /* current position */
  , unsafe_mapping : vector<int> /* memory */
  }

/* Represents the brainfuck code */
type program := 
  { program_pointer  : int /* where we are in the code currently */
  , instruction_size : int /* lenght of the instructions */
  , instructions     : vector<char> /* instructions already tokenized */
  }

/* The interpreter environment that will hold everything we need */
type interpreter_env := 
  { mem        : memory 
  , p          : program 
  , nestedness : int /* keep track of how many inested '['/']' we've seen. Useful for loops */

  }

/* Expand the memory with a 0. */
unit expand_memory(interpreter_env env by reference){
  env.mem.size := env.mem.size + 1;
  append(env.mem.unsafe_mapping,0);
  return unit;
}

/* Increments the memory pointer by 1, while also expanding the memory */
unit increment_memory_pointer(interpreter_env env by reference){
  env.mem.memory_pointer := env.mem.memory_pointer + 1;
  expand_memory(env);
  return unit;
}

/* Decrements the memory pointer if able, otherwise, it stays the same. */
unit decrement_memory_pointer(interpreter_env env by reference){
  match env.mem.memory_pointer with 
    0 => {return unit;}
    n => {
      env.mem.memory_pointer := n - 1;
      return unit;
    }
  ;
}

/* increments the instruction pointer to the next token */
bool increment_program_pointer(interpreter_env env by reference){
  env.p.program_pointer := env.p.program_pointer + 1;
  return env.p.program_pointer >= env.p.instruction_size; 
}

/* decrements the instruction pointer to the previous token */
unit decrement_program_pointer(interpreter_env env by reference){
  env.p.program_pointer := env.p.program_pointer - 1;
  return unit;
}

/* increments the value of the current cell */
unit increment_memory_cell(interpreter_env env by reference){
  env.mem.unsafe_mapping[env.mem.size] := env.mem.unsafe_mapping[env.mem.size ] + 1;
  return unit;
}

/* decrements the value of the current cell */
unit decrement_memory_cell(interpreter_env env by reference){
  env.mem.unsafe_mapping[env.mem.size ] := mem.unsafe_mapping[env.mem.size ] - 1;
  return unit;
}

/* prints the current cell */
unit print_current_cell(interpreter_env env by reference){
  print(to_string(env.mem.unsafe_mapping[env.mem.size ]));
  return unit;
}


/* ask for input and sets the current cell */
unit set_current_cell(interpreter_env env by reference){
  env.mem.unsafe_mapping[env.mem.size ] := to_int(input("> "));
  return unit;
}

/* skip tokens till you get to the closing ']' */
unit skip_till_close(interpreter_env env by reference){
  match env.p.instructions[env.p.program_pointer] with 
    ']' => { /* we saw a close */
      match env.nestedness with  /* we gotta check if it closes the one we want or a nested one */
        1 => { 
          /* closes the one we want, so we sanitize the state, increment the pointer and return */
          env.p.nestedness := env.p.nestedness == 0;
          increment_program_pointer(env);
          return unit;
        }
        n => {
          /* closes a nested '[]', incremenet the program counter, decrement the nestedness, continue */
          env.p.nestedness := env.p.nestedness - 1;
          increment_program_pointer(env);
          skip_till_close(env);
          return unit;
        }
      ;
    }
    '[' => { /* we saw an open, aka: nestedness */
      /* increment nestedness level and continue */
      env.p.nestedness := env.p.nestedness + 1;
      increment_program_pointer(env);
      skip_till_close(env);
      return unit;
    }
    _   => { /* we saw any other token */
      /* ignore it and continue */
      increment_program_pointer(env);
      skip_till_close(env);
      return unit;
    }
  ;
}

/* loop forward, aka: '[' */
unit loop_forward(interpreter_env env by reference){
  match env.mem.unsafe_mapping[env.mem.size - 1] with 
    0 => {return skip_till_close(env);}
    _ => {return unit;}
  ;
}


/* skip tokens till you get to the opening '[' */
unit skip_till_open(interpreter_env env by reference){
  match env.p.instructions[env.p.program_pointer] with 
    '[' => { /* we saw an opening */
      match env.nestedness with  /* we gotta check if it opens the one we want or a nested one */
        0 => { 
          /* opens the one we want, so we sanitize the state, increment the pointer and return */
          env.p.nestedness := env.p.nestedness == 0;
          increment_program_pointer(env);
          return unit;
        }
        n => {
          /* opens a nested '[]', decrement the program counter, decrement the nestedness, continue */
          env.p.nestedness := env.p.nestedness - 1;
          decrement_program_pointer(env);
          skip_till_open(env);
          return unit;
        }
      ;
    }
    ']' => { /* we saw an close, aka: nestedness */
      /* increment nestedness level and continue */
      env.p.nestedness := env.p.nestedness + 1;
      decrement_program_pointer(env);
      skip_till_open(env);
      return unit;
    }
    _   => { /* we saw any other token */
      /* ignore it and continue */
      decrement_program_pointer(env);
      skip_till_open(env);
      return unit;
    }
  ;
}

/* loop backward, aka: ']' */
unit loop_backward(interpreter_env env by reference){
  match env.mem.unsafe_mapping[env.mem.size - 1] with 
    0 => {return unit;}
    _ => {return skip_till_open(env);}
  ;
}


unit interpret(interpreter_env env by reference){


  match env.p.instructions[env.p.program_pointer]  with
    '>' => {
      increment_memory_pointer(env);
    }
    '<' => {
      decrement_memory_pointer(env);
    }
    '+' => {
      increment_memory_cell(env);
    }
    '-' => {
      decrement_memory_cell(env);
    }
    '.' => {
      print_current_cell(env);
    }
    ',' => {
      set_current_cell(env);
    }
    '[' => {
      loop_forward(env); 
    }
    ']' => {
      loop_backward(env);
    }
  ;
  /* End of input? */
  match increment_program_pointer(env) with 
    true  => {return unit;}
    false => {return interpret(env);}
  ;
}

unit tokenize(string cs, vector<char> tokens by reference){
  for (c : cs){
    append(tokens,c);
  };

}

int main(){
  string code := ",+[-.,+]";
  vector<char> tokens := new vector<char>();
  tokenize(tokens);
  vector<int> umemory := [0];
  memory  m := new memory(0,0,umemory);
  program pr := new program(0,8,tokens);
  interpreter_env env := new interpreter_env(m,pr,0);
  interpret(env);
  return 0;
}

