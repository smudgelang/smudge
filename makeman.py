#!/usr/bin/env python3

def get_readme_section(filename, start="BEGIN", end="END"):
    with open(filename, 'r') as readme:
        summary = []
        lines = readme.readlines()
        section = None
        for line in lines:
            if section is None and start in line:
                section = line.split()[line.split().index(start) + 1]
                summary.append('[={0}]\n'.format(section))
            elif section:
                if end in line:
                    section = None
                else:
                    if line.startswith('##'):
                        line = line.strip('##')
                        line = '.B ' + line + '\n'
                    summary.append(line)
        return ''.join(summary)

if __name__=='__main__':
    summary = get_readme_section('README.md')
    with open('manpage.in', 'w') as manpage:
        name = '\n[=NAME]\n\nsmudge - smudge programming language\n'
        manpage.write(name)
        author = '\n[=AUTHOR]\nWritten by Nate Bragg, Nathan Michaels\n'
        manpage.write(summary)
