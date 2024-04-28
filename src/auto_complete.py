import re
def modify_variables_based_on_comments(script):
    lines = script.split('\n')
    modified_lines = []
    i = 0
    while i < len(lines):
        line = lines[i]
        
        if line.strip().startswith('//'):
            comments = []
            while i < len(lines) and lines[i].strip().startswith('//'):
                comments.append(lines[i])
                i += 1

            if i < len(lines):  
                next_line = lines[i]
                for comment in comments:
                    if "should use `=` to be matched." in comment:
                        matches = re.findall(r"\{([^}]*)\}", comment)
                        if matches:
                            for var in matches[0].split(','):
                                var = var.strip().strip("'")
                                next_line = re.sub(rf"\b{var}\b", f"={var}", next_line)

                    if "are unbounded" in comment:
                        matches = re.findall(r"\{([^}]*)\}", comment)
                        if matches:
                            for var in matches[0].split(','):
                                var = var.strip().strip("'")
                                next_line = re.sub(rf"\b{var}\b", f"'{var}'", next_line)
                
                modified_lines.append(next_line) 
                i += 1  
                continue  

        modified_lines.append(line)
        i += 1

    return '\n'.join(modified_lines)
if __name__ == "__main__":
    new_spec = """\
let Alice(idA, idB, Kas) = 
  new Na;
  out(senc(<idA,idB,Na>, Kas));
  in(cypher1);
  let <Na, idB, Kab, message2> = sdec(cypher1, Kas) in
  out(message2);
  in(cypher2);
  let Nb = sdec(cypher2, Kab) in
  out(senc(Nb, Kab)); 0
let Bob(idB, Kbs, idA) = 
  in(cypher3);
  let <Kab, idA> = sdec(cypher3, Kbs) in
  new Nb;
  out(senc(Nb, Kab));
  in(cypher4);
  let Nb = sdec(cypher4, Kab) in 0
let S(idA, idB, Kas, Kbs) = 
  let <idA, idB, Na> = sdec(cypher5, Kas) in
  new Kab;
  let message1 = senc(<Na,idB,Kab,senc(<Kab,idA>, Kbs)>, Kas) in
  out(message1); 0
"""
    from gpt.analysizer import analysis
    print(modify_variables_based_on_comments(analysis(new_spec)))