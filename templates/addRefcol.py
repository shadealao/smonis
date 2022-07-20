#!/usr/bin/env python

if __name__ == "__main__":
    
    import sys
    import os

    for arg in sys.argv:
        print arg

    inputFileName = sys.argv[1]
    outputFileName, fileExt = os.path.splitext(inputFileName)
    outputFileName += "-with-colref" + fileExt

    print("Start treatement ...")

    outputFile = open(outputFileName, "w")
    inputFile = open(inputFileName, "r")

    inputLines = inputFile.readlines()
    outputLines = []

    i = 0
    j = 0
    beforeTableBody = True
    for line in inputLines:
        if beforeTableBody:
            if line.find("<div .table-body") != -1:
                beforeTableBody = False

            tableRowPos = line.find("<div .table-row")
            colHeaderPos = line.find("<div .column-header")
            if tableRowPos != -1:
                newline = line.replace(".table-row", ".table-row .column-headers")
                print newline
            elif colHeaderPos != -1 and line.find("-id") != -1:
                newline = line.replace(".column-header", ".col-" + str(i) + " .column-header")
                #print "col -id " + str(i)
                print newline
                i += 1
            elif colHeaderPos != -1 and line.find("-id") == -1 and line.find(".action-icon") == -1: 
                newline = line.replace(".column-header", ".colref .col-" + str(i) + " .column-header")
                #print "colref " + str(i)
                print newline
                i += 1
            elif colHeaderPos != -1 and line.find("-id") == -1 and line.find(".action-icon") != -1: 
                newline = line.replace(".column-header", ".col-" + str(i) + " .column-header")
                #print "action-icon " + str(i)
                print newline
                i += 1
            else:
                newline = line

        else: # afterTableBody
            colPos = line.find("<div .")
            if colPos != -1:
                newline = line.replace("<div .", "<div .col-" + str(j) + " .")
                print newline
                j += 1
            else:
                newline = line
                
        outputLines.append(newline)
            
    outputFile.writelines(outputLines)
    
    outputFile.close()
    inputFile.close()

    print("Finished treatement OK")

    

    
