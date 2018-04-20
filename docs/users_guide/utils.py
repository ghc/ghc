from docutils import nodes

# Taken from Docutils source inside the ListTable class. We must bypass
# using the class itself, but this function comes in handy.
def build_table_from_list(table_data, col_widths):
    table = nodes.table()
    tgroup = nodes.tgroup(cols=len(col_widths))
    table += tgroup
    for col_width in col_widths:
        colspec = nodes.colspec(colwidth=col_width)
        tgroup += colspec
    rows = []
    for row in table_data:
        row_node = nodes.row()
        for cell in row:
            entry = nodes.entry()
            entry += cell
            row_node += entry
        rows.append(row_node)
    thead = nodes.thead()
    thead.extend(rows[:1])
    tgroup += thead
    tbody = nodes.tbody()
    tbody.extend(rows[1:])
    tgroup += tbody
    return table
