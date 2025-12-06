import sys

def file_dimensions(fname,newfname):
    with open(fname) as file:
        lengths = [len(line.strip()) for line in file.readlines()]
    with open(newfname,"w") as file:
        file.write(f"{len(lengths)}\n")
        file.write(f"{max(lengths)}\n")

def write_file(fname,newfname):
    with open(fname) as file:
        lines = [line.strip() for line in file.readlines()]

    # replace characters

    with open(newfname,"a") as file:
        for line in lines:
            file.write(f"{line}\n")

if __name__ == "__main__":
    fname = sys.argv[1]
    newfname = f"../fortran/{fname}"
    
    file_dimensions(fname,newfname)
    write_file(fname,newfname)
