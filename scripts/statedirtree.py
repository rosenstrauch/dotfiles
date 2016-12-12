#!/usr/bin/python
import os
import sys

tree = []
def main(argv):

    # extract path to an attachment directory (remove *.txt extension)
    path = argv[1]

    # extract page name from a path
    page = str(path.split("/")[-1:][0])
    # construct output directory (deprecated)
    path = "%s" % (path)
    targetfile = os.path.join(path, 'index.txt')

    showFolderTree(path,show_files=True,indentation=4,file_output=(targetfile))

def showFolderTree(path,show_files=False,indentation=2,file_output=False):
    """
    Shows the content of a folder in a tree structure.
    path -(string)- path of the root folder we want to show.
    show_files -(boolean)-  Whether or not we want to see files listed.
                            Defaults to False.
    indentation -(int)- Indentation we want to use, defaults to 2.
    file_output -(string)-  Path (including the name) of the file where we want
                            to save the tree.
    """


    tree = []


    if not show_files:
        for root, dirs, files in os.walk(path):
            level = root.replace(path, '').count(os.sep)
            indent = ' '*indentation*(level)
            tree.append('{}{}/'.format(indent,os.path.basename(root)))

    if show_files:
        for root, dirs, files in os.walk(path):
            level = root.replace(path, '').count(os.sep)
            indent = ' ' * 4 * (level)
            print('{}{}/'.format(indent, os.path.basename(root)))
            subindent = ' ' * 4 * (level + 1)
            for f in files:
                print('{}{}'.format(subindent, f))

    if file_output:
        output_file = open(file_output,'w')
        for line in tree:
            output_file.write(line)
    else:
        # Default behaviour: print on screen.
        for line in tree:
            print (line)

if __name__ == "__main__":
    main(sys.argv)
