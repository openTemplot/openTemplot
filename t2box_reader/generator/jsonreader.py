import json
import sqlite3
import sys

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

# This class to be subclassed to provide functionality required as the JSON nodes are read
class handler():
    def initial_data():
        raise NotImplementedError()

    def do_start_record(self, record, level):
        raise NotImplementedError()
    def do_end_record(self, record, level):
        raise NotImplementedError()
    def do_array():
        raise NotImplementedError()
    def do_boolean():
        raise NotImplementedError()
    def do_byte():
        raise NotImplementedError()
    def do_double():
        raise NotImplementedError()
    def do_float():
        raise NotImplementedError()
    def do_integer():
        raise NotImplementedError()
    def do_long():
        raise NotImplementedError()
    def do_single():
        raise NotImplementedError()
    def do_extended():
        raise NotImplementedError()


# === Open JSON file ===
class JSON_reader():
    def __init__(self, path):
        print("Opening JSON ...")
        try:
            file = open(path)
            self.base_record = json.load(file)
            print("    ... was successful")
            file.close()
        except Error as e:
            print(f"    ... raised an error: {e}")
            throw(e)

    def process(self, handler, data):
        self.process_record(self.base_record, handler, data)

# === _____ ===

    def process_record(self, record, handler, data):
        data = handler.do_start_record(record, data)
        data = self.process_fields(record['fields'], handler, data)
        data = handler.do_end_record(record, data)
        return(data)

    def process_fields(self, fields, handler, data):
        for fld in fields:
            data = self.process_field(fld, handler, data)
        return(data)

    def process_field(self, field, handler, data):
        global types
        if field['type'][0] == '.':
            data = self.process_record(field, handler, data)
        else:
            if   field['type'] == '4':
                data = handler.do_integer(field, data)
            elif field['type'] == 'a':
                data = handler.do_array(field, data)
            elif field['type'] == 'b':
                data = handler.do_byte(field, data)
            elif field['type'] == 'd':
                data = handler.do_double(field, data)
            elif field['type'] == 'f':
                data = handler.do_float(field, data)
            elif field['type'] == 'o':
                data = handler.do_boolean(field, data)
            elif field['type'] == 's':
                data = handler.do_string(field, data)
            elif field['type'] == 'x':
                data = handler.do_extended(field, data)
            else:
                print("Unknown field type: ", field)
                throw(exception("Unknown field type"))
        return(data)


    def process_array(self, number, name, fld_type, comment):
        # arr_bnds = {'swtimbco_c': 42,
                    # 'psleep_c':   51}
        match = re.match('array\s*\[(\d+)\.\.([^\]]*)\]\s*of\s*(.*)', fld_type)
        if match == None:
            print("Unmatched array defn","'" + fld_type + "'")
            sys.exit()
        lower = match.group(1)
        upper = match.group(2)
        fld_type = match.group(3)
        print('for ix :=', lower, 'to', upper, 'do')

