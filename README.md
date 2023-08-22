# HSS_SUB

This software, comprising of two FORTRAN90 code components, models the temperature-history dependent nature of high-strength steels.


----------------------------------------------------------------------------------------------------------------------------------------
# TO USE THIS CODE:

## Part 0: Code variable adjustment
   You may need to calibrate the code to your own material properties.

   Under HSS_SUB.for: SUBROUTINE vuhard:
   - LINEAR_TO_FRACTURE  : Set this to true to model plastic stress decreasing to zero at strains above ε_t. Set this to false to maintain a perfectly plastic response of f_t at strains above ε_t.
   - KELVIN              : Set this to true if the model temperatures are in Kelvin, or false if the model temperatures are in Celcius.
   - Lookup tables       : These can be modified to suit the steel being modelled. Subscript _TEMP denotes the relevantlookup  temperature for each value
     - lutkb             : bolt reduction factor k_b. By default from Eurocode 3 Part 1.2
     - LUTE              : elastic modulus reduction factor k_E. By default from Eurocode 3 Part 1.2
     - LUTKP             : proportional limit factor. By default from Behaviour of Grade 8.8 bolts under natural fire conditions—Tests and model, F. Hanus, G. Zilli, J.-M. Franssen (2011)
     - LUTEP_U           : Maximum strain. By default from Behaviour of Grade 8.8 bolts under natural fire conditions—Tests and model, F. Hanus, G. Zilli, J.-M. Franssen (2011)
     - EP_Y_INTERP_STRAIN: Yield strain. By default interpolates the yield strain from 0.02 to 0.1 between maximum temperatures of 600 °C to 800 °C
   - F_T_LIM             : Limiting stress. By default is 500 MPa from Behaviour of Grade 8.8 bolts under natural fire conditions—Tests and model, F. Hanus, G. Zilli, J.-M. Franssen (2011)

   Under fFVtoNT.f:
   - VERBOSE             : This is a debugging variable. Set to true to output every time a new increment is read and written. This significantly slows the processing speed.
       
----------------------------------------------------------------------------------------------------------------------------------------
## Part 1: Thermal analysis
   This part is not necessary if the mechanical analysis is to be conducted at a known current and maximum temperature. 
1. Ensure Abaqus is able to compile and run user FORTRAN code.
2. Place fFVtoNT.for into the Abaqus working directory (usually C:\Temp)
3. Compile fFVtoNT.for in Abaqus/Command using the line 'abaqus make job=fFVtoNT'
4. Produce a decoupled thermo-mechanical model as usual.
5. OPTIONAL: Add all high-strength steels to a single set. If your model uses multiple materials, this should reduce the storage required for the results file.
6. In your decoupled thermal analysis, define two Predefined Fields:
   1. An initial Field variable for field variable 1 (FV1) and magnitude equal to your initial temperature.
   2. A user defined field variable for FV1 in the thermal analysis step(s).
7. Include the following Keyword under **OUTPUT REQUESTS in the input file
```
** 
** OUTPUT REQUESTS
** 
*EL FILE, POSITION=AVERAGED AT NODES
FV
```
  If you are using a named set for your high-strength-steel nodes, use
```
** 
** OUTPUT REQUESTS
** 
*EL FILE, ELSET=[setname], POSITION=AVERAGED AT NODES
FV
```
8. Ensure the field and results outputs occur at the same frequency. This can be achieved by ensuring the field output is produced at each iteration.
9. OPTIONAL: include FV in the field output or history requests.
10. Attach HSS_SUB.for to the job file as the User subroutine file.
11. Run the thermal analysis.

----------------------------------------------------------------------------------------------------------------------------------------
## Part 2: Convert thermal analysis results

1. Run the command 'abaqus fFVtoNT' in Abaqus/Command
2. Input the job name as prompted by the program. The program will convert the field variable records from [jobname-thermal].fil into nodal temperature records in [jobname-thermal].fin.
4. Confirm that the last step, number of increments, step time, and total time agree with the thermal analysis
5. The program will have created a new [jobname-thermal].fin file in the working directory. Rename this file to [jobname-thermal].fil; you may need to rename/delete the existing .fil file.

----------------------------------------------------------------------------------------------------------------------------------------
## Part 3: Mechanical Analysis

1. Produce a mechanical model as usual. Ensure the mesh is compatible with the thermal analysis.
2. Define two predefined fields:
  1. A Read from Results or Database for nodal temperatures using the [jobname-thermal].obd file. This is done as normal for a decoupled thermal-mechanical analysis.
  2. A Read from Results or Database for field variable 1 using the [jobname-thermal].fil file. Set the output variable to NT (ABAQUS/CAE will not proceed if this is empty).
  3. If the testing and maximum temperatures are known (i.e. an isothermal analysis), then the nodal and field temperatures can be directly specified. Skip to step 3.
3. If the analysis is transient temperature, then from the following keyword block:
```
** 
** PREDEFINED FIELDS
** 
** Name: [name]   Type: Field
*Field, variable=1, file=[jobname-thermal].fil, OUTPUT VARIABLE=NT, bstep=1
```
   Delete the parameter OUTPUT VARIABLE=NT

4. Define a new material for the high-strength-steel.
5. Add a new 'Mechanical > Plasticity > Plastic' behaviour. Set 'Hardening' to User.
6. Input two properties as follows:
   1. Initial ambient maximum strength f_y
   2. Initial ambient elastic modulus E. Ensure this value is the same as the initial ambient value that will be in the 'Elastic' behaviour. 
7. Include any other necessary behaviours (such as Elasticity).
8. Attach HSS_SUB.for to the job file as the User subroutine file.
9. Run the mechanical analysis

   
