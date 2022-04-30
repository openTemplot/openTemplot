import struct
import sys
import box_dump_fields

# --- Parameters ---
fname = None    # Name of the input file
xlen = None     # Length of an 'extended' float. Defaults to 10 but can be set to 8

# --- CONSTANTS ---
PRINTABLE_RANGE = range(32,127)

helptext = [
    "",
    "USAGE: python3  box_dump.py  FNAME  [, EXTENDED_LENGTH]",
    "",
    "FNAME - the name of the file to be dumped",
    "",
    "EXTENDED_LENGTH - how extended floats are viewed:",
    "           x or 10 - 10-byte intel format (default)",
    "           d or 8  - 8 byte 'double' floats",
    ""]


def help():
    for line in helptext:
        print(line)
    exit()


def get_parms():
    global fname, xlen

    if len(sys.argv) < 2:
        help()
    fname = sys.argv[1]

    if len(sys.argv) < 3:
        xlen = 10
    else:
        xlen = sys.argv[2]
        if xlen == '10' or xlen == 'x':
            xlen = 10
        elif xlen == '8' or xlen == 'd':
            xlen = 8
        else:
            help()


def get_data():
    global fname
    infile =  open(fname, 'rb')
    try :
        data = infile.read()
        return data
    except:
        print("Error reading file", fname)
        exit()


def parse_int(data, offset):
    intbytes = data[offset:offset+4]
    i = int.from_bytes(intbytes, 'little', signed=True)
    return i, offset + 4

def parse_string_by_len(data, offset, strlen):
    try:
        newoffset = offset + strlen
        strbytes = data[offset : newoffset]
        string = strbytes.decode('cp1252')
    except Exception as e:
        print("At offset:", offset, '*** error *** : '+str(e))
        exit()
    else:
        return(string, newoffset)


def fmt_addr(addr):
    return "{:06x}".format(addr)

def fmt_line(data, base, offset):
    rslt = fmt_addr(base)
    for i in range(0,16):               # accumulate hex values
        if (i >= offset) and (i < len(data)):
            rslt += " {:02x}".format(data[i])
        else:
            rslt += "   "
        if i % 4 == 3:
            rslt += " "

    rslt += " | "

    for i in range(0,16):               # accumulate printable values
        if (i >= offset) and (i < len(data)):
            if data[i] in PRINTABLE_RANGE:
                rslt += chr(data[i])
            else:
                rslt += "."
        else:
            rslt += " "
    rslt += " | "
    return rslt


def print_range(data, fromix, printlen):

    # First print any 'header' (i.e. partial block)
    blockbase = fromix >> 4 << 4
    skiplen = fromix - blockbase        # How many bytes to skip for the header?
    if skiplen > 0:                     # if more than none, then print the header
        block = data[blockbase:blockbase+16]
        print(fmt_line(block, blockbase, skiplen))
        blockbase += 16

    printed = skiplen

    while (printed < printlen) and (blockbase < len(data))  :
        block = data[blockbase:blockbase+16]
        print(fmt_line(block, blockbase, 0))
        blockbase += 16
        printed += 16

def print_fld(name, offset, value):
        print(fmt_addr(offset), name.ljust(32), value)


def do_str(name, data, offset, length):
    global last_box
    bytecount = data[offset]
    strbytes = data[offset+1 : offset+bytecount+1]
    try:
        string = strbytes.decode('cp1252')
    except Exception as e:
        print_fld(name, offset, '*** error *** : '+str(e))
    else:
        print_fld(name, offset, string)
    if name == 'box_ident':
        print("==> box ident", string)
        if string[0:2] == 'NX':
            last_box = True
    return offset + length + 1


def do_byte(name, data, offset, dummy):
    b = data[offset]
    print_fld(name, offset, b)
    return offset + 1


def do_int(name, data, offset, dummy):
    i, newoffset = parse_int(data, offset)
    print_fld(name, offset, i)
    return newoffset


def do_bool(name, data, offset, dummy):
    b = data[offset]
    print_fld(name, offset, b)
    return offset + 1

def do_single(name, data, offset, dummy):
    fbytes = data[offset : offset+4]
    [f] = struct.unpack('f', fbytes)
    print_fld(name, offset, f)
    return offset + 4

def do_double(name, data, offset, dummy):
    fbytes = data[offset : offset+8]
    [f] = struct.unpack('d', fbytes)
    print_fld(name, offset, f)
    return offset + 8


def do_ext(name, data, offset, dummy):
    global xlen
    if xlen == 8:
        return do_double(name, data, offset, dummy)
    else:
        try:
            extbytes = data[offset : offset+10]             # Grab the relevant bytes
            mantbytes = extbytes[0:8]                       # get teh mantissa butes
            sign = extbytes[9] >> 7                         # extract the sign
            exponent = (((extbytes[9]  & 0x7f)  << 8) +     # get the (biassed) exponent
                        extbytes[8])
            if exponent == 0:
                value = 0                                   # zero exponent means number == 0
            else:
                exponent -= 16383                           # subtract the exponent bias
                mantissa = int.from_bytes(mantbytes, 'little', signed=False)
                value = mantissa / pow(2, 63)               # Calculate the value
                value *= pow(2, exponent)                   #
            if sign:
                value *= -1
            print_fld(name, offset, value)
            return offset + 10
        except Exception as e:
            return do_dump(name, data, offset, xlen)


def do_dump(name, data, offset, count):
#    strbytes = data[offset : offset+count]
#    print(name, offset, strbytes)
    print_fld(name, offset, '')
    print_range(data, offset, count)
    return offset + count


def do_field(name, fld_type, count, data, offset):
    func = None
    if fld_type == 'boolean':
        func = do_bool
    elif fld_type == 'byte':
        func = do_byte
    elif fld_type == 'double':
        func = do_double
    elif fld_type == 'extended':
        func = do_ext
    elif fld_type == 'h':
        func = do_dump
    elif fld_type == 'integer':
        func = do_int
    elif fld_type == 'float':
        func = do_single
    elif fld_type == 'single':
        func = do_single
    elif fld_type == 'string':
        func = do_str
    elif fld_type == '>':
        print("\t>>> Start of", name)
        return offset
    elif fld_type == '<':
        print("\t<<< End of", name)
        return offset
    else:
        raise Exception("unknown type " + fld_type)
    newoffset = func(name, data, offset, count)
    return newoffset


# ===========================================================

get_parms()
data = get_data()

print("Data is", len(data), "bytes long\n")
print("Expecting", xlen, "byte floats\n")

offset = 0
template_count = 0
separator = "\n================================================================================================\n"

# ==== First read templates ====
last_box = False


while not last_box:
    print(separator)
    for fld in box_dump_fields.fields(xlen):
        offset = do_field(fld[0], fld[1], fld[2], data, offset)
    template_count += 1

print(template_count, "templates")
print(separator)

# ==== then the strings ====

stringlen, offset = parse_int(data, offset)
print("Stringlen =", stringlen)
string, offset = parse_string_by_len(data, offset, stringlen)
for line in string.split('|'):
    print(line)
print(separator)

stringlen, offset = parse_int(data, offset)
print("Stringlen =", stringlen)
string, offset = parse_string_by_len(data, offset, stringlen)
for line in string.split('|'):
    print(line)
print(separator)

# ==== then the data blocks ====

# blocks_delim = "_85A_|."
# string, offset = parse_string_by_len(data, offset, 8)
# if string[0:7] != blocks_delim:
    # print("Data block delimiter string '" + blocks_delim + "' missing!")
    # offset -= 8
    # print_range(data, offset, 2048)
    # exit()
    

# ==== finally dump the next bit ====
print_range(data, offset, 2048)


