#auxiliar functions
def _isInRowspan(y, x, rowspan):
    rowspan_value = 0
    row_i = 0
    for i in range(y):
        if (i, x) in rowspan.keys():
            rowspan_value = rowspan[(i, x)]
            row_i = i
    if rowspan_value - (y - row_i) > 0:
        return True
    else:
        return False

def _writeCell(table, y, x, length, rowspan = {}):
    text = table[y][x]
    extra_spaces = ""
    if _isInRowspan(y, x, rowspan):
        text = "|"
        for i in range(length): #according to column width
            text += " "
        print(text, end = "")
    else:
        for i in range(length - len(text) - 2):
            extra_spaces += " " #according to column width
        print(f"| {text} " + extra_spaces, end = "")

def _writeColspanCell(length, colspan_value): #length argument refers to sum of column widths
    text = ""
    for i in range(length + colspan_value - 1):
        text += " "
    print(text, end = "")

def _getMaxColWidth(table, idx): #find the longest cell in the column to set the column's width
    maxi = 0
    for row in table:
        if len(row) > idx: #avoid index out of range error
            cur_len = len(row[idx]) + 2
            if maxi < cur_len:
                maxi = cur_len
    return maxi

def _getMaxRowLen(table): #find longest row list (in terms of elements)
    maxi = 0
    for row in table:
        cur_len = len(row)
        if maxi < cur_len:
            maxi = cur_len
    return maxi

def _getAllColLen(table): #collect in a list the widths of each column
    widths = [_getMaxColWidth(table, i) for i in range(_getMaxRowLen(table))]
    return widths

def _getMaxRowWidth(table): #set the width of the table
    maxi = 0
    for i in range(len(table)):
        cur_len = sum(_getAllColLen(table)) + len(_getAllColLen(table)) + 1 # "|" at borders and between cells
        if maxi < cur_len:
            maxi = cur_len
    return maxi

def _drawBorder(table, y, colspan = {}, rowspan = {}):
    col_widths = _getAllColLen(table)
    length = _getMaxRowWidth(table)
    cell_w_count = 0
    cell_counter = 0
    for i in range(length):
        if _isInRowspan(y, cell_counter - 1, rowspan) and not (i == cell_w_count or i == length - 1):
            print(" ", end = "")
        elif i == cell_w_count or i == length - 1:
            print("+", end = "")
            if cell_counter != _getMaxRowLen(table):
                cell_w_count += col_widths[cell_counter] + 1
                cell_counter += 1
        else:
            print("-", end = "")
    print("") #next line (end = "\n")

def _convertTable(table): #turns all table elements into strings
    for i in range(len(table)):
        for j in range(len(table[i])):
            table[i][j] = str(table[i][j])
    return table

#main function
def tabulate(tab, colspan = {}, rowspan = {}):
    table = _convertTable(tab)
    table_width = _getMaxRowWidth(table)
    col_widths = _getAllColLen(table)
    for y, row in enumerate(table):
        _drawBorder(table, y, colspan, rowspan)
        x = 0
        while x < len(row): #altered for loop
            _writeCell(table, y, x, col_widths[x], rowspan)
            if (y, x) in colspan.keys():
                colspan_value = colspan[(y, x)]
                _writeColspanCell(sum(col_widths[x+1:x+colspan_value]), colspan_value)
                x += colspan_value - 1
            x += 1
        print("|") #end table row
    _drawBorder(table, _getMaxRowLen(table) - 1) #close bottom of table

