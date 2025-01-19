# FP_reveng
 Reverse Engineering with FreePascal
 
## Goal
This is a prototype project designed to answer this simple question: 

*Starting from a Windows 64-bit executable file, is it possible to write a Free Pascal application that performs the exact same function as the original executable?*

Here is the idea:

* Start with a Windows 64-bit executable file.
* Decompile the executable file with IDAfree and export the result as an ASM file.
* Parse the ASM file with FP_reveng to produce a PAS file.
* Load and compile the PAS file with Free Pascal to produce an executable file that performs the exact same function as the original executable.

This serves as a starting point for applying any modifications you need to the PAS file.

## Disclaimer
I am aware that probably, it isn't possible to achieve the goal, and that perhaps all of this is just a waste of time.

## the path

I want to keep the ASM code as intact as possible while translating it into Pascal procedures. That makes sense, especially if you're aiming to preserve the logic and structure of the original code with minimal modifications.

Typically, IDA Pro renders an ASM procedure in a way that might look something like this:

```
; =============== S U B R O U T I N E =======================================
sub_140001110   proc near               ; DATA XREF: .rdata:0000000142287398↓o
                                        ; .pdata:000000014362F00C↓o
                sub     rsp, 28h
                xor     edx, edx        ; dwSpinCount
                lea     rcx, CriticalSection ; lpCriticalSection
             ............
loc_140001154:                          ; CODE XREF: sub_140001110+15↑j
                                        ; sub_140001110:loc_14000113B↑j
                lea     rcx, sub_142258930
                mov     cs:dword_142F25010, 48h ; 'H'
                add     rsp, 28h
                jmp     sub_1403FBA34
sub_140001110   endp

; ---------------------------------------------------------------------------
```
the procedure is roughly translated as:
```
procedure sub_140001110; assembler; 
asm 
   sub     rsp, 28h
   xor     edx, edx        ; dwSpinCount
   lea     rcx, CriticalSection ; lpCriticalSection
             ............
label:loc_140001154;   // CODE XREF: sub_140001110+15↑j
                                    //sub_140001110:loc_14000113B↑j
   lea     rcx, sub_142258930
   mov     cs:dword_142F25010, 48h // 'H'
   add     rsp, 28h
   jmp     sub_1403FBA34
end;
```

Note that I do not try to extract any parameters from the procedure, because I do not need to translate the asm code into pascal code, I simply want to obtain a working pascal code while minimizing the risk of altering the original code.
# program flow
While decoding the asm code, there are three program states:

* reading comments
* reading procedure
* reading global data structure 
## reading procedure
The procedure starts with a row that starts with these three tokens:

``` 
<proc_name> proc near
```
The procedure ends with a line that starts with two tokens:

```
<proc_name> endp
```

## reading comments
That happend when we aren't reading procedures or global data structures. The whole row is a comment.








