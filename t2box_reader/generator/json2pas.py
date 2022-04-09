import json
import sqlite3
import sys
import jsonreader

types= {
    '4':'integer',
    '8':'long',
    'a':'array',
    'b':'byte',
    'd':'double',
    'f':'float',
    'o':'boolean',
    's':'single',
    'x':'extended',
}

prelude = """{
                          ===== WARNING =====

    This include file is generated.
    It should not be edited by hand unless the idea of generating it is being
    abandoned for good. Once the generation process is discarded
    there will probably be no going back.
    This may be fine - just make sure.  ;-)
}"""


class pasgen_handler(jsonreader.handler):
    def prefix(self, level):
        return("  " * level)

    def initial_data(self):
        outfilename = "load_t2keep_data.inc"
        outfile = open(outfilename, 'w')
        print(r'%s' % prelude, file=outfile)
        return({'outfile': outfile,
                'level': 0,
                'path':[]})

    def do_start_record(self, record, data):
        print(self.prefix(data['level'])+'//', data['level'], record['number'], " >>> ", record['name'], file=data['outfile'])
        data['level'] += 1
        if record['name'] != '':
            data['path'].append(record['name'])
        # print(data['path'], file=data['outfile'])
        return(data)

    def do_end_record(self, record, data):
        data['level'] -= 1
        data['path'] = data['path'][:-1]
        # print(data['path'], file=data['outfile'])
        print(self.prefix(data['level'])+'//', data['level'], record['number'], " <<< ", record['name'], file=data['outfile'])
        return(data)

    def printit(self, level, path, fldname, fldtype, extra = 0):
        if extra > 0:
            print(self.prefix(level) + '.'.join(path)+'.'+fldname+" := parse_"+fldtype+"(box_file, '"+fldname+"', "+str(extra)+");", file=data['outfile'])
        elif extra == -1:
            print(self.prefix(level) + '.'.join(path)+'.'+fldname+"[ix] := parse_"+fldtype+"(box_file, '"+fldname+"['+inttostr(ix)+']');", file=data['outfile'])
        else:
            print(self.prefix(level) + '.'.join(path)+'.'+fldname+" := parse_"+fldtype+"(box_file, '"+fldname+"');", file=data['outfile'])
        return(data)

    def do_array(self, field, data):
        if field['basetype'] != "Told_shove":
            print(self.prefix(data['level']), "for ix :=", field['lower'], "to", field['upper']+"-1", "do begin", file=data['outfile'])
            if field['basetype'] in types:
                self.printit(data['level']+1, data['path'], field['name'], types[field['basetype']], -1)
            else:
                self.printit(data['level']+1, data['path'], field['name'], field['basetype'], -1)
            print(self.prefix(data['level']+1), "if",  '.'.join(data['path'])+'.', field['name']+"[ix]", "= 0 then break;", file=data['outfile']),
            print(self.prefix(data['level']), "end;", file=data['outfile']),

            print(self.prefix(data['level']), "for ix := ix to", field['upper']+"-1", "do begin", file=data['outfile']),
            if field['basetype'] in types:
                self.printit(data['level']+1, data['path'], field['name'], types[field['basetype']], -1)
            else:
                self.printit(data['level']+1, data['path'], field['name'], field['basetype'], -1)
            print(self.prefix(data['level']), "end;", file=data['outfile'])

        else:
            print(self.prefix(data['level']), "for ix :=", field['lower'], "to", field['upper'], "do begin", file=data['outfile'])
             # //      parse_Told_shove(box_file, 'shoves['+inttostr(ix)+']');
            print(self.prefix(data['level']+1), "parse_integer(box_file, 'sv_code['+inttostr(ix)+']');;     // 0=empty slot, -1=omit this timber,  1=shove this timber.", file=data['outfile'])
            print(self.prefix(data['level']+1), "parse_string(box_file, 'sv_str['+inttostr(ix)+']',8);   // timber number string.", file=data['outfile'])
            print(self.prefix(data['level']+1), "parse_byte(box_file, 'sv_alignment_byte['+inttostr(ix)+']');   // D5 0.81 12-06-05", file=data['outfile'])
            print(self.prefix(data['level']+1), "parse_extended(box_file, 'sv_x['+inttostr(ix)+']');;    // xtb modifier.", file=data['outfile'])
            print(self.prefix(data['level']+1), "parse_extended(box_file, 'sv_k['+inttostr(ix)+']');;    // angle modifier.", file=data['outfile'])
            print(self.prefix(data['level']+1), "parse_extended(box_file, 'sv_o['+inttostr(ix)+']');;    // offset modifier (near end).", file=data['outfile'])
            print(self.prefix(data['level']+1), "parse_extended(box_file, 'sv_l['+inttostr(ix)+']');;    // length modifier (far end).", file=data['outfile'])
            print(self.prefix(data['level']+1), "parse_extended(box_file, 'sv_w['+inttostr(ix)+']');;    // width modifier (per side).", file=data['outfile'])
            print(self.prefix(data['level']+1), "parse_integer(box_file, 'sv_t['+inttostr(ix)+']');;     // nyi - thickness modifier in 1000ths of mm. (was spare integer).", file=data['outfile'])
            print(self.prefix(data['level']), "end;", file=data['outfile']),

        return(data)

        
    def do_boolean(self, field, data):
        self.printit(data['level'], data['path'], field['name'], "boolean")
        return(data)
    def do_byte(self, field, data):
        self.printit(data['level'], data['path'], field['name'], "byte")
        return(data)
    def do_double(self, field, data):
        self.printit(data['level'], data['path'], field['name'], "double")
        return(data)
    def do_extended(self, field, data):
        self.printit(data['level'], data['path'], field['name'], "extended")
        return(data)
    def do_float(self, field, data):
        self.printit(data['level'], data['path'], field['name'], "float")
        return(data)
    def do_integer(self, field, data):
        self.printit(data['level'], data['path'], field['name'], "integer")
        return(data)
    def do_long(self, field, data):
        raise NotImplementedError()
        return(data)
    def do_string(self, field, data):
        self.printit(data['level'], data['path'], field['name'], "string", field['strlen'])
        return(data)


#==============================================================

if len(sys.argv) < 2:
    recname = 'Told_keep_data.json'
    print("JSON file name missing. Defaulting to", recname)
else:
    recname = sys.argv[1]

handler = pasgen_handler()
data = handler.initial_data()
reader = jsonreader.JSON_reader(recname)
reader.process(handler, data)

print("___ DONE ___")

