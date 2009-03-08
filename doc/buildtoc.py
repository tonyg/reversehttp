import sys

lines = sys.stdin.readlines()
toc = []

def depthfor(h):
    depth = 0
    while h[depth] == '#':
        depth = depth + 1
    return depth

def printtoc():
    for (depth, h) in toc:
        if depth == 2:
            label = h[depth:].strip()
            sys.stdout.write('  ' * (depth - 1))
            sys.stdout.write('- [%s](#%s)\n' % (label, anchorfor(h)))

def anchorfor(line):
    result = ""
    for c in line:
        if c.isalnum():
            result = result + c.lower()
    return result

for line in lines:
    if line.startswith('#'):
        toc.append((depthfor(line), line))

for line in lines:
    if line.startswith('@TOC@'):
        printtoc()
    elif line.startswith('#'):
        depth = depthfor(line)
        sys.stdout.write('#' * depth)
        sys.stdout.write('<a name="%s"></a>%s' % (anchorfor(line), line[depth:]))
    else:
        sys.stdout.write(line)
