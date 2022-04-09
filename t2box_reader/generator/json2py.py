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

class pygen_handler(jsonreader.handler):
    def prefix(self, level):
        return("  " * level)

    def initial_data(self):
        return({'level': 0,
                'path':[]})

    def do_start_record(self, record, data):
        print(self.prefix(data['level']) + "['"+record['name']+"', '>',  0],")
        data['level'] += 1
        if record['name'] != '':
            data['path'].append(record['name'])
        # print(data['path'])
        return(data)

    def do_end_record(self, record, data):
        data['level'] -= 1                       
        data['path'] = data['path'][:-1]
        # print(data['path'])
        print(self.prefix(data['level']) + "['"+record['name']+"', '<',  0],")
        return(data)

    def do_array(self, field, data):
        print(self.prefix(data['level']) + "['"+field['name']+"', '------> array["+field['lower']+"-"+field['upper']+"] of "+field['basetype'],  0)
        # # print(self.prefix(data['level']), data['level'], data['path'], field['number'], field['name'], "array [", field['lower'], field['upper'], "] of", types[field['basetype']])
        # print(self.prefix(data['level']), "for ix :=", field['lower'], "to", field['upper'], "do begin"),
        # if field['basetype'] in types:
            # self.printit(data['level']+1, data['path'], field['name'], types[field['basetype']], -1)
        # else:
            # self.printit(data['level']+1, data['path'], field['name'], field['basetype'], -1)
        # print(self.prefix(data['level']+1), "if",  '.'.join(data['path'])+'.', field['name']+"[ix]", "= 0 then break;"),
        # print(self.prefix(data['level']), "end;"),
# 
        # print(self.prefix(data['level']), "for ix := ix to", field['upper'], "do begin"),
        # if field['basetype'] in types:
            # self.printit(data['level']+1, data['path'], field['name'], types[field['basetype']], -1)
        # else:
            # self.printit(data['level']+1, data['path'], field['name'], field['basetype'], -1)
        # print(self.prefix(data['level']), "end;"),
        return(data)
        
    def do_boolean(self, field, data):
        print(self.prefix(data['level']) + "['"+field['name']+"', 'boolean',  0],")
        return(data)
    def do_byte(self, field, data):
        print(self.prefix(data['level']) + "['"+field['name']+"', 'byte',  0],")
        return(data)
    def do_double(self, field, data):
        print(self.prefix(data['level']) + "['"+field['name']+"', 'double',  0],")
        return(data)
    def do_extended(self, field, data):
        print(self.prefix(data['level']) + "['"+field['name']+"', 'extended',  0],")
        return(data)
    def do_float(self, field, data):
        print(self.prefix(data['level']) + "['"+field['name']+"', 'float',  0],")
        return(data)
    def do_integer(self, field, data):
        print(self.prefix(data['level']) + "['"+field['name']+"', 'integer',  0],")
        return(data)
    def do_long(self, field, data):
        print(self.prefix(data['level']) + "['"+field['name']+"', 'longd',  0],")
        raise NotImplementedError()
        return(data)
    def do_string(self, field, data):
        print(self.prefix(data['level']) + "['"+field['name']+"', 'string', "+str(field['strlen'])+"],")
        return(data)


if len(sys.argv) < 2:
    recname = 'Told_keep_data.json'
    print("JSON file name missing. Defaulting to", recname)
else:
    recname = sys.argv[1]


handler = pygen_handler()
data = handler.initial_data()
reader = jsonreader.JSON_reader(recname)
reader.process(handler, data)

print("___ DONE ___")

