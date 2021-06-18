/*
File name: buffer.c
Compiler: MS Visual Studio 2019
Author: Divine Omorogieva, ID# 040897820
Course: CST 8152 – Compilers, Lab Section: 013
Assignment: 1
Date: June 8th, 2020
Professor: Svillen . Ranev
Purpose: C file for the buffer program - a buffer descriptor that can operate in three different modes
Function list: b_allocate, b_addc, b_clear, b_free, b_isfull, b_addcoffset, b_capacity, b_markc, b_mode,
               b_incfactor, b_load, b_isempty, b_getc, b_eob, b_print
*/
#include <stdlib.h>
#include <limits.h>
#include "buffer.h"

/*
Purpose: Create a new Buffer in the memory
Author: Divine Omorogieva
History/Versions: 1.0
Called functions: calloc(), sizeof(), malloc(), free()
Parameters: short init_capacity, char inc_factor,char o_mode
Return value: pointerBuffer or NULL
Algorithm: - Allocate memory for one Buffer structure
           - Allocate memory for one dynamic character, based on the initial capacity given
           - Set the Buffer mode operator and inc_factor, based on the given o_mode and inc_factor
           - Return the pointer to Buffer if succeeded, else free dynamically allocated memory and
             return NULL
*/
Buffer* b_allocate(short init_capacity, char inc_factor, char o_mode) {
    Buffer* pointerBuffer; 
    pointerBuffer = (Buffer*)calloc(1, sizeof(Buffer));

    if (pointerBuffer == NULL)
        return NULL;

    if (init_capacity < 0 || init_capacity > MAX_CAPACITY) {
        free(pointerBuffer);
        pointerBuffer = NULL;
        return NULL;
    }



    if (init_capacity == 0) {

        pointerBuffer->cb_head = (char*)malloc(sizeof(char) * DEFAULT_INIT_CAPACITY);

        if (pointerBuffer->cb_head == NULL) {
            free(pointerBuffer);
            pointerBuffer = NULL;
            return NULL;
        }

        if (o_mode == 'a') {
            pointerBuffer->inc_factor = INC_FACTOR_A_M;
            pointerBuffer->mode = MODE_ADD;
        }
        else if (o_mode == 'm') {
            pointerBuffer->inc_factor = INC_FACTOR_A_M;
            pointerBuffer->mode = MODE_MULTI;
        }
        else if (o_mode == 'f') {
            pointerBuffer->inc_factor = INC_FACTOR_F;
            pointerBuffer->mode = MODE_FIXED;
        }
        else {
            free(pointerBuffer->cb_head);
            pointerBuffer->cb_head = NULL;
            free(pointerBuffer);
            pointerBuffer = NULL;
            return NULL;
        }
        pointerBuffer->capacity = DEFAULT_INIT_CAPACITY;
    }

    if (init_capacity != 0) {
        pointerBuffer->cb_head = (char*)malloc(sizeof(char) * init_capacity);
        if (pointerBuffer->cb_head == NULL) {
            free(pointerBuffer);
            return NULL;
        }

        if (o_mode == 'f' || (unsigned char)inc_factor == INC_FACTOR_F) {
            pointerBuffer->mode = MODE_FIXED;
            pointerBuffer->inc_factor = INC_FACTOR_F;
        }
        else if (o_mode == 'a' && (unsigned char)inc_factor <= INC_FACTOR_ADD_MAX && (unsigned char)inc_factor >= INC_FACTOR_ADD_MIN) {
            pointerBuffer->mode = MODE_ADD;
            pointerBuffer->inc_factor = inc_factor;
        }
        else if (o_mode == 'm' && (unsigned char)inc_factor <= INC_FACTOR_MULTI_MAX && (unsigned char)inc_factor >= INC_FACTOR_MULTI_MIN) {
            pointerBuffer->mode = MODE_MULTI;
            pointerBuffer->inc_factor = inc_factor;
        }
        else {
            free(pointerBuffer->cb_head);
            pointerBuffer->cb_head = NULL;
            free(pointerBuffer);
            pointerBuffer = NULL;
            return NULL;
        }

        pointerBuffer->capacity = init_capacity;
    }



    pointerBuffer->flags = pointerBuffer->flags & DEFAULTZ;
    pointerBuffer->flags = pointerBuffer->flags | DEFAULT_FLAGS;

    return pointerBuffer;
}

/*
Purpose: Set the r_flag bit to 0/1 and add a new character to the Buffer. Resize if needed.
Author: Divine Omorogieva
History/Versions: 1.0
Called functions: realloc()
Parameters: pBuffer const pBD, char symbol
Return value: pBuffer pBD
Algorithm: - Set the r_flag bit to 0 using bit wise.
           - If the buffer is not full, add the symbol and increment addc_offset by one
           - If the buffer is full, resize the buffer based on the operation mode.
           - Return pointer to the Buffer if succeeded, else return NULL and free dynamically allocated memory
*/
pBuffer b_addc(pBuffer const pBD, char symbol) {

    short newCapacity;
    short availableSpace;
    short newIncrement;
    char* newBuffer; 
    char* oldBuffer;
    newCapacity = 0;

    if (pBD == NULL) {
        return NULL;
    }


    pBD->flags &= RESET_R_FLAG;


    if ((short)(pBD->addc_offset * sizeof(char)) == pBD->capacity) {


        if (pBD->mode == MODE_FIXED) {
            return NULL;
        }


        if (pBD->mode == MODE_ADD) {

            newCapacity = pBD->capacity + (unsigned char)pBD->inc_factor;

            if (newCapacity > 0 && newCapacity > MAX_CAPACITY) {
                newCapacity = MAX_CAPACITY;
            }
            else if (newCapacity < 0) {
                return NULL;
            }
        }


        if (pBD->mode == MODE_MULTI) {

            if (pBD->capacity == MAX_CAPACITY) {
                return NULL;
            }


            availableSpace = MAX_CAPACITY - pBD->capacity;
            newIncrement = (short)(availableSpace * ((float)pBD->inc_factor / 100));
            newCapacity = pBD->capacity + newIncrement;


            if (newIncrement == 0 && pBD->capacity < MAX_CAPACITY) {
                newCapacity = MAX_CAPACITY;
            }
            else if (newCapacity > MAX_CAPACITY || newCapacity < 0) {
                return NULL;
            }
        }


        oldBuffer = pBD->cb_head;
        newBuffer = (char*)realloc(pBD->cb_head, newCapacity);


        if (newBuffer == NULL) {
            return NULL;
        }


        if (newBuffer != oldBuffer) {
            pBD->flags |= SET_R_FLAG;
        }


        pBD->capacity = newCapacity;
        pBD->cb_head = newBuffer;

    }

    pBD->cb_head[pBD->addc_offset] = symbol;
    pBD->addc_offset++;
    return pBD;
}

/*
Purpose: Clear the Buffer and retain all memory space currently allocatd to the buffer
Author: Divine Omorogieva
History/Versions: 1.0
Called functions: None
Parameters: Buffer* const pBD
Return value: 1 or -1
Algorithm: - Check if the pointer to the Buffer is working, else return -1
           - Reset the Buffer addc_offset, flags, getc_offset, markc_offset. Return 1 to notify success.
*/
int b_clear(Buffer* const pBD) {

    if (pBD == NULL) {
        return RT_FAIL_1;
    }


    pBD->flags = pBD->flags & DEFAULTZ;
    pBD->flags = pBD->flags | DEFAULT_FLAGS;
    pBD->getc_offset = 0;
    pBD->markc_offset = 0;
    pBD->addc_offset = 0;

    return TRUE;
}

/*
Purpose: Free all memory inside the Buffer
Author: Divine Omorogieva
History/Versions: 1.0
Called functions: free()
Parameters: Buffer* const pBD
Return value: None
Algorithm: - Check if the pointer is NULL. If not, free the dynamically allocate memory for the
           character array and the Buffer structure.
*/
void b_free(Buffer* const pBD) {

    if (pBD->cb_head != NULL) {
        free(pBD->cb_head);
        pBD->cb_head = NULL;
    }

    free(pBD);
}

/*
Purpose: Check the capacity of the character buffer
Author: Divine Omorogieva
History/Versions: 1.0
Called functions: None
Parameters: Buffer *const pBD
Return value: 1, 0, -1
Algorithm: - Check if the pointer to the Buffer is working, if not, return -1
           - Check the value of the Buffer capacity. If it is full return 1, if not return 0
*/
int b_isfull(Buffer* const pBD) {

    if (pBD == NULL) {
        return RT_FAIL_1;
    }


    if ((short)(pBD->addc_offset * sizeof(char)) == pBD->capacity) {
        return TRUE;
    }

    return FALSE;
}

/*
Purpose: Check the limit of the character buffer
Author: Divine Omorogieva
History/Versions: 1.0
Called functions: None
Parameters: Buffer *const pBD
Return value: short addc_offset
Algorithm: - Check if the pointer to the Buffer is working, if not, return -1
           - Check the value of the Buffer addc_offset, return the value.
*/
short b_addcoffset(Buffer* const pBD) {

    if (pBD == NULL) {
        return RT_FAIL_1;
    }

    return pBD->addc_offset;
}


/*
Purpose: Return the capacity of the character buffer
Author: Divine Omorogieva
History/Versions: 1.0
Called functions: None
Parameters: Buffer *const pBD
Return value: short capacity
Algorithm: - Check if the pointer to the Buffer is working, if not, return -1
           - Check the value of the Buffer capacity. Return the value
*/
short b_capacity(Buffer* const pBD) {

    if (pBD == NULL) {
        return RT_FAIL_1;
    }

    return pBD->capacity;
}


/*
Purpose: Set markc_offset to mark
Author: Divine Omorogieva
History/Versions: 1.0
Called functions: None
Parameters: pBuffer const pBD
Return value: short markc_offset, -1
Algorithm: - Check if the pointer to the Buffer is working, if not, return -1
           - Check the value of the mark param. If it is within the limt of the buffer, return markc_offset, else return -1
*/
short b_markc(pBuffer const pBD, short mark) {

    if (pBD == NULL) {
        return RT_FAIL_1;
    }


    if (mark >= 0 && mark <= pBD->addc_offset) {
        pBD->markc_offset = mark;
        return pBD->markc_offset;
    }

    return RT_FAIL_1;
}


/*
Purpose: Return the value of mode
Author: Divine Omorogieva
History/Versions: 1.0
Called functions: None
Parameters: Buffer *const pBD
Return value: 1, int mode
Algorithm: - Check if the pointer to the Buffer is working, if not, return -1
           - Check the value of the Buffer capacity. If it is full return 1, if not return 0
*/
int b_mode(Buffer* const pBD) {

    if (pBD == NULL) {
        return RT_FAIL_1;
    }

    return (int)pBD->mode;
}

/*
Purpose: Return the non-negative value of inc_factor
Author: Divine Omorogieva
History/Versions: 1.0
Called functions: None
Parameters: Buffer *const pBD
Return value: inc_factor or 0x100 (256 in size_t)
Algorithm: - Check if the pointer to the Buffer is working, if not, return 0x100
           - Check the value of the Buffer inc_factor. Return the value inc_factor (casted to size_t)
*/
size_t b_incfactor(Buffer* const pBD) {

    if (pBD == NULL) {
        return (size_t)INC_FACTOR_FAIL;
    }

    return (unsigned char)(pBD->inc_factor);
}

/*
Purpose: Load an open input file into the Buffer
Author: Divine Omorogieva
History/Versions: 1.0
Called functions: fgetc(), feof(), ungetc(), b_addc()
Parameters: Buffer *const pBD, FILE* const fi
Return value: -1, -2 or numbers of character added to the Buffer (num)
Algorithm: - Check the Buffer pointer and the file pointer. Return -1 if failed
           - Read the file and add the character to the Buffer. Return -2 if unable to added
             , else return the number of characters added to the Buffer
*/
int b_load(FILE* const fi, Buffer* const pBD) {
    char c; 
    int num; 
    num = 0;

    
    if (pBD == NULL || fi == NULL) {
        return RT_FAIL_1;
    }

    
    while (1) {

        c = (char)fgetc(fi);


        if (feof(fi)) {
            break;
        }


        if (b_addc(pBD, c) == NULL) {
            ungetc(c, fi);
            return LOAD_FAIL;
        }
        num++;
    }

    return num;
}

/*
Purpose: Return the empty state of the Buffer
Author: Divine Omorogieva
History/Versions: 1.0
Called functions: None
Parameters: Buffer *const pBD
Return value: 1, 0, -1
Algorithm: - Check if the pointer to the Buffer is working, if not, return -1
           - Check the value of the addc_offset. If it is 0, return 1, other wise return 0.
*/
int b_isempty(Buffer* const pBD) {

    if (pBD == NULL) {
        return RT_FAIL_1;
    }


    if (pBD->addc_offset == 0) {
        return TRUE;
    }

    return FALSE;
}

/*
Purpose: Read the Buffer
Author: Divine Omorogieva
History/Versions: 1.0
Called functions: None
Parameters: Buffer *const pBD
Return value: -2, 0
Algorithm: - Check if the pointer to the Buffer is working, if not, return -2
           - Check the value of getc_offset and addc_offset, set flags field eob to 1, return 0. Otherwise,
           set eob to 0
           - Return character located at getc_offset, increment getc_offset by 1
*/
char b_getc(Buffer* const pBD) {

    if (pBD == NULL) {
        return RT_FAIL_2;
    }


    if (pBD->getc_offset == pBD->addc_offset) {
        pBD->flags |= SET_EOB;
        return 0;
    }
    else {
        pBD->flags &= RESET_EOB;
    }


    return pBD->cb_head[pBD->getc_offset++];
}

/*
Purpose: Return the value of flags field eob bit.
Author: Divine Omorogieva
History/Versions: 1.0
Called functions: None
Parameters: Buffer *const pBD
Return value: Value of flags determined by the eob bit or -1
Algorithm: - Check if the pointer to the Buffer is working, if not, return -1
           - Check the value of flag determined by the eob bit, using bitwise operation
*/
int b_eob(Buffer* const pBD) {

    if (pBD == NULL) {
        return RT_FAIL_1;
    }

    return (int)pBD->flags & CHECK_EOB;
}

/*
Purpose: Print character of the Buffer one by one. For diagnostic purpose only
Author: Divine Omorogieva
History/Versions: 1.0
Called functions: b_eob(), b_getc(), b_isempty()
Parameters: Buffer *const pBD, char nl
Return value: Return the number of characters printed, else return -1 on failure
Algorithm: - Check if the pointer to the Buffer is working, if failed, return -1
           - In a while loop, use b_getc() to get the character from the character array. Use
           b_eob() to detect the end of the buffer and break the loop. Print the character to
           the standard output.
           - Return the numbers of characters printed. If specified, print out a new line character
*/
int b_print(Buffer* const pBD, char nl) {
    int num; 
    char c;
    num = 0; 

    if (pBD == NULL || pBD->cb_head == NULL) {
        return RT_FAIL_1;
    }

    while (1) {

        c = b_getc(pBD);


        if (b_eob(pBD) == 2) {
            break;
        }


        printf("%c", c);
        num++;
    }


    if (nl != 0)
        printf("\n");

    return num;
}

/*
Purpose: Use realloc() to adjust the new capacity and update all necessary features of the Buffer descriptor
Author: Divine Omorogieva
History/Versions: 1.0
Called functions: realloc(), sizeof()
Parameters: Buffer *const pBD, char symbol
Return value: Return the new pointer to the Buffer, else return NULL
Algorithm: - Check if the pointer to the Buffer or the cb_head is working, if failed, return NULL
           - Using realloc() to increase the size of cb_head array, and add the symbol to the end of the
           Buffer character array, increment addc_offset by one.
*/
Buffer* b_compact(Buffer* const pBD, char symbol) {
    short newCapacity; 
    char* oldBuffer;
    char* newBuffer; 
    newCapacity = 0; 


    if (pBD == NULL || pBD->cb_head == NULL) {
        return NULL;
    }


    newCapacity = (short)(sizeof(char) * (pBD->addc_offset + 1));


    if (newCapacity <= 0) {
        return NULL;
    }


    oldBuffer = pBD->cb_head;
    newBuffer = (char*)realloc(pBD->cb_head, newCapacity);


    if (newBuffer == NULL) {
        return NULL;
    }


    if (newBuffer != pBD->cb_head) {
        pBD->flags |= SET_R_FLAG;
    }


    pBD->cb_head = newBuffer;
    pBD->cb_head[pBD->addc_offset] = symbol;
    pBD->addc_offset++;
    pBD->capacity = newCapacity;
    return pBD;
}

/*
Purpose: returns the value of flags determined by the r_flag bit
Author: Divine Omorogieva
History/Versions: 1.0
Called functions: none
Parameters: Buffer *const pBD
Return value: Return the value of the flags field, if error, return -1
Algorithm: - Check if the pointer to the Buffer, if failed, return -1
           - Using bitwise operation to return the value of the flags field.
*/
char b_rflag(Buffer* const pBD) {

    if (pBD == NULL) {
        return RT_FAIL_1;
    }

    return (char)(pBD->flags & CHECK_R_FLAG);
}

/*
Purpose: Decrements the value of getc_offset by one
Author: Divine Omorogieva
History/Versions: 1.0
Called functions: none
Parameters: Buffer *const pBD
Return value: getc_offset, -1
Algorithm: - Check the pointer to the Buffer and getc_offset, if failed, return -1
           - Decrement getc_offset by one, return getc_offset
*/
short b_retract(Buffer* const pBD) {

    if (pBD == NULL || pBD->getc_offset <= 0) {
        return RT_FAIL_1;
    }

    pBD->getc_offset--;
    return pBD->getc_offset;
}

/*
Purpose: Set the getc_offset to the value of the current markc_offset
Author: Divine Omorogieva
History/Versions: 1.0
Called functions: none
Parameters: Buffer *const pBD
Return value: getc_offset, -1
Algorithm: - Check if the pointer to the Buffer, if failed, return -1
           - Set the value of getc_offset to markc_offset, returns getc_offset if no problem occurs.
           - If markc_offset smaller than 0 or larger than addc_offset, return -1
*/
short b_reset(Buffer* const pBD) {

    if (pBD == NULL || pBD->markc_offset < 0 || pBD->markc_offset > pBD->addc_offset) {
        return RT_FAIL_1;
    }

    pBD->getc_offset = pBD->markc_offset;
    return pBD->getc_offset;
}

/*
Purpose: Return getc_offset value to the calling function
Author: Divine Omorogieva
History/Versions: 1.0
Called functions: none
Parameters: Buffer *const pBD
Return value: getc_offset, -1
Algorithm: - Check if the pointer to the Buffer, if failed, return -1
           - Returns getc_offset if no problem occurs.
           - If getc_offset smaller than 0 or larger than addc_offset, return -1
*/
short b_getcoffset(Buffer* const pBD) {

    if (pBD == NULL || pBD->getc_offset < 0 || pBD->getc_offset > pBD->addc_offset) {
        return RT_FAIL_1;
    }

    return pBD->getc_offset;
}

/*Purpose: Set the getc_offset and markc_offset to 0 so that it can reread again.
Author: Divine Omorogieva
History/Versions: 1.0
Called functions: none
Parameters: Buffer *const pBD
Return value: 0, -1
Algorithm: - Check if the pointer to the Buffer, if failed, return -1
           - Set the value of getc_offset and markc_offset to 0, returns 0 if no problem occurs.
*/
int b_rewind(Buffer* const pBD) {

    if (pBD == NULL) {
        return RT_FAIL_1;
    }

    pBD->getc_offset = 0;
    pBD->markc_offset = 0;
    return 0;
}

/*Purpose: Return a pointer to a location of the character buffer indicated by the current markc_offset
Author: Divine Omorogieva
History/Versions: 1.0
Called functions: none
Parameters: Buffer *const pBD
Return value: a pointer to a character or NULL
Algorithm: - Check if the pointer to the Buffer and the cb_head, if failed, return NULL
           - Return the value of the character at markc_offset
           - If markc_offset smaller than 0 or larger than addc_offset, return NULL
*/
char* b_location(Buffer* const pBD, short loc_offset) {

    if (pBD == NULL || pBD->cb_head == NULL || pBD->markc_offset < 0 || pBD->markc_offset > pBD->addc_offset) {
        return NULL;
    }

    return pBD->cb_head + loc_offset;
}
