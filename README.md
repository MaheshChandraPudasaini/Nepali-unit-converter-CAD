# Nepali-unit-converter
Arc_gis Nepali area unit converter from Field Calculator
How to use in gis 

in field calculator:
calculate Area 
Add Field > Give name > type= Text >  length=8
Field Calculator:
copy:
def R_Ana(A):
    R=(A/508.74)
    r=int(R)
    A=(R-r)*16.0
    a=int(A)    
    P=(A-a)*4.0    
    p=int(P)
    D=int((P-p)*4.0)
    a=[r,a,p,D]
    return ("-".join([str(i) for i in a]))
Parser (script)=python
check box Show Codeblock
pest in pre-logic Script Code
In Function box call function as field_name(Area_field name)
Or
Load directly .cal file on field calculator
