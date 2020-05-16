#define NAME "JEV"
#define _BSD_SOURCE
#define _GNU_SOURCE


#include <termios.h>
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/ioctl.h> /*Helps us get the size of the terminal*/
#include <sys/time.h> /* Time stamp for our message*/
#include <unistd.h>
#include <stdarg.h> 
#include <fcntl.h>
//imported libraries


/* Syntax highlight types */
#define HL_NORMAL 0
#define HL_NONPRINT 1
#define HL_COMMENT 2   /* Single line comment. */
#define HL_MLCOMMENT 3 /* Multi-line comment. */
#define HL_KEYWORD1 4
#define HL_KEYWORD2 5
#define HL_STRING 6
#define HL_NUMBER 7
#define HL_MATCH 8      /* Search match. */



#define HL_HIGHLIGHT_STRINGS (1<<0)
#define HL_HIGHLIGHT_NUMBERS (1<<1)


struct editorSyntax {
   char **filematch;
   char **keywords;
   char singleline_comment_start[2];
   char multiline_comment_start[3];
   char multiline_comment_end[3];
   int flags;
};


/* This structure represents a single line of the file we are editing. */

typedef struct erow {
//erow represents a new line of text 
   int idx;            /* Row index in the file, zero-based. */
   int size;           /* Size of the row, excluding the null term. */
   int rsize;          /* Size of the rendered row. */
   char *chars;        /* Row content. */
   char *render;       /* Row content "rendered" for screen (for TABs). */
   unsigned char *hl;  /* Syntax highlight type for each character in render.*/
   int *bd;  /* Bold type for each character in render.*/
   int hl_oc;          /* Row had open comment at end in last syntax highlight
                          check. */
} erow;


struct editorConfig {
   int cx,cy;  /* Cursor x and y position in characters */
   int rowoff;     /* Offset of row displayed. */
   int coloff;     /* Offset of column displayed. */
   int screenrows; /* Number of rows that we can show */
   int screencols; /* Number of cols that we can show */
   int numrows;    /* Number of rows */
   int rawmode;    /* Is terminal raw mode enabled? */
   int bolded;     /* Bolded if 1, unbolded if 0 */
   erow *row;      /* Rows */ //allows for multiple lines
  //editor config stores a row object too that can get size etc
   int dirty;      /* File modified but not saved. */
//counts unsaved changes made
   char *filename; /* Currently open filename */
   char statusmsg[80];
   time_t statusmsg_time;
   char *copied_char_buffer;
   int *copied_bd_buffer;
   struct editorSyntax *syntax;    /* Current syntax highlight, or NULL. */
//filewords, keywords, multiline comments etc stuff to highlight

};


static struct editorConfig E; 
//global variable holding editor state, E holds all above variables

enum KEY_ACTION{
       KEY_NULL = 0,       /* NULL */
       CTRL_B=2,           /* Ctrl-b */
       CTRL_C = 3,         /* Ctrl-c */
       CTRL_D = 4,         /* Ctrl-d */
       CTRL_F = 6,         /* Ctrl-f */
       CTRL_H = 8,         /* Ctrl-h */
       CTRL_I=9,
       CTRL_K=11,
       TAB = 9,            /* Tab */
       CTRL_L = 12,        /* Ctrl+l */
       ENTER = 13,         /* Enter */
       CTRL_Q = 17,        /* Ctrl-q */
       CTRL_S = 19,        /* Ctrl-s */
       CTRL_U = 21,        /* Ctrl-u */
       CTRL_V=22,
       ESC = 27,           /* Escape */
       BACKSPACE =  127,   /* Backspace */
       /* The following are just soft codes, not really reported by the
        * terminal directly. */

       ARROW_LEFT = 1000,


       ARROW_RIGHT,


       ARROW_UP,


       ARROW_DOWN,


       DEL_KEY,


       HOME_KEY,


       END_KEY,


       PAGE_UP,


       PAGE_DOWN


};


void editorSetStatusMessage(const char *fmt, ...);
char *C_HL_extensions[] = {".c",".cpp",".h",".java",".py",NULL};
char *C_HL_keywords[] = {
       /* A few C / C++ keywords */

       "switch","if","while","for","break","continue","return","else",
       "struct","union","typedef","static","enum","class",
       /* C types */
       "int|","long|","double|","float|","char|","unsigned|","signed|",
       "void|",NULL
};


struct editorSyntax HLDB[] = {
   {
       /* C / C++ */
       C_HL_extensions,
       C_HL_keywords,
       "//","/*","*/",
       HL_HIGHLIGHT_STRINGS | HL_HIGHLIGHT_NUMBERS
   }
}; 
//all the things that need to be highlighted, stored in an struct of editorSyntax type 


#define HLDB_ENTRIES (sizeof(HLDB)/sizeof(HLDB[0]))
/* ======================= Low level terminal handling ====================== */

static struct termios orig_termios; /* In order to restore at exit.*/


void disableRawMode(int fd) {
   /* Don't even check the return value as it's too late. */
   if (E.rawmode) {
       tcsetattr(fd,TCSAFLUSH,&orig_termios); //sets terminal to original settings when done and TCSAFLUSH discards unread input*/
       E.rawmode = 0;
   }
}
//Called at exit to avoid remaining in raw mode. 
void editorAtExit(void) {
   disableRawMode(STDIN_FILENO);
}
//when editor is done, snaps out of raw mode 


int enableRawMode(int fd) {
   struct termios raw;
   if (E.rawmode) return 0;
//if the raw mode variable has already been set, do nothing
   if (!isatty(STDIN_FILENO)) return -1;
//isatty returns ‘1’ if the file is not referring to the terminal. So if the file is not referring to the terminal, then that is a problem and will goto fatal 
   atexit(editorAtExit);//atexit is called immediately at exit of program, exits rawmode
   if (tcgetattr(fd,&orig_termios) == -1) return -1; //if the fd file cannot be stored in orig_termios, give error 
   raw = orig_termios;  //raw is the modifiable version of the original termios settings 
   /* input modes: no break, no CR to NL, no parity check, no strip char,
    * no start/stop output control. */
   raw.c_iflag &= ~(BRKINT | ICRNL | INPCK | ISTRIP | IXON);
//these bolded ones are not necessarily needed; can probably be deleted 
   /* output modes - disable post processing */
   raw.c_oflag &= ~(OPOST);
//disable carriage control, doesn’t put cursor at beginning of new line 
   /* control modes - set 8 bit chars */
   raw.c_cflag |= (CS8);
   /* local modes - choing off, canonical off, no extended functions,
    * no signal chars (^Z,^C) */
   raw.c_lflag &= ~(ECHO | ICANON | IEXTEN | ISIG); //bitwise arithmetic; takes BIT echo, NOT it with ~ and then & that with ‘&’ get 0’s
//ECHO off makes you not see text ‘echoed’ in terminal
//ICANON off reads byte by byte not line by line 
//ISIG off lets things like ctrl z and s to not be taken as such controls but simply be read as bytes,not signals 
//IEXTREN off disables ctrl-o stuff
   /* control chars - set return condition: min number of bytes and timer. */
   raw.c_cc[VMIN] = 0; /* Return each byte, or zero for timeout. */
//return as SOON as there is any input to be read, that’s why it is set to 0
   raw.c_cc[VTIME] = 1; /* 100 ms timeout (unit is tens of second). */
//max amount of time before ‘read’ returns
   /* put terminal in raw mode after flushing */
   if (tcsetattr(fd,TCSAFLUSH,&raw) < 0) return -1;
//The tcsetattr() function sets the parameters associated with the terminal referred to by the specified file descriptor from the termios structure; TCSAFLUSH makes sure that happens after all file output has been transmitted
   E.rawmode = 1;
//shows that rawmode has been activated
   return 0;

}
//error giving piece of code 

void clearScreen()  //Clears the screen
{
  const char *CLEAR_SCREEN_ANSI = "\e[1;1H\e[2J";
  write(STDOUT_FILENO, CLEAR_SCREEN_ANSI, 12);
}

/* Read a key from the terminal put in raw mode, trying to handle
* escape sequences. */
int editorReadKey(int fd) { 
//read one key press and return it 
   int nread;
   char c, seq[3];
   while ((nread = read(fd,&c,1)) == 0);
//reads file to the char c address until it can no longer do so (not equalling 0)
   if (nread == -1) exit(1); //if there is nothing to read, give an exit error 
   while(1) {
       switch(c) {
       case ESC:    /* escape sequence */
           /* If this is just an ESC, we'll timeout here. */
           if (read(fd,seq,1) == 0) return ESC;
//ESC is the 27 escape sequence detailed above 
           if (read(fd,seq+1,1) == 0) return ESC;
//if there is nothing to read after the first and second bytes, esc
           /* ESC [ sequences. */
           if (seq[0] == '[') {
               if (seq[1] >= '0' && seq[1] <= '9') {
                   /* Extended escape, read additional byte. */
                   if (read(fd,seq+2,1) == 0) return ESC;
                   if (seq[2] == '~') {
                       switch(seq[1]) {
                       case '3': return DEL_KEY;
                       case '5': return PAGE_UP;
                       case '6': return PAGE_DOWN;
                       }
                   }
               } else {
                   switch(seq[1]) {
                   case 'A': return ARROW_UP;
                   case 'B': return ARROW_DOWN;
                   case 'C': return ARROW_RIGHT;
                   case 'D': return ARROW_LEFT;
                   case 'H': return HOME_KEY;
                   case 'F': return END_KEY;
                   }//gets all the ESC sequences for when 127[.... Is typed
               }
           }
           /* ESC O sequences. */

           else if (seq[0] == 'O') {

               switch(seq[1]) {
               case 'H': return HOME_KEY;
               case 'F': return END_KEY;
               }
           }
           break;
       default:
           return c;
       }
   }
}


/* Use the ESC [6n escape sequence to query the horizontal cursor position
* and return it. On error -1 is returned, on success the position of the
* cursor is stored at *rows and *cols and 0 is returned. */
int getCursorPosition(int ifd, int ofd, int *rows, int *cols) {
//called in getWindowSize 
   char buf[32];
   unsigned int i = 0;
/* Report cursor location */
   if (write(ofd, "\x1b[6n", 4) != 4) return -1;
//6n queries the terminal for the location of the cursor, read reply from the ofd input; response is 4 digits: an escape sequence of 27[ and then the actual response 
   /* Read the response: ESC [ rows ; cols R */

   while (i < sizeof(buf)-1) {
       if (read(ifd,buf+i,1) != 1) break;
       if (buf[i] == 'R') break;
       i++;
   }
   buf[i] = '\0';
//last thing in buf is a null terminator character
   /* Parse it. */
   if (buf[0] != ESC || buf[1] != '[') return -1;
//if the first two bytes aren’t the escape code ones, error
   if (sscanf(buf+2,"%d;%d",rows,cols) != 2) return -1;
   return 0;
}

int getWindowSize(int ifd, int ofd, int *rows, int *cols) {
   struct winsize ws;
   if (ioctl(1, TIOCGWINSZ, &ws) == -1 || ws.ws_col == 0) {
       return -1;
   } else {
       *cols = ws.ws_col;
       *rows = ws.ws_row;
       return 0;
//does this else statement only when ioctl gives win size and writes it to ws
   }
}

/* ====================== Syntax highlight color scheme  ==================== */

int is_separator(int c) {
   return c == '\0' || isspace(c) || strchr(",.()+-/*=~%[];",c) != NULL;
} 
//checks if something is a separator character or null terminator 
/* Return true if the specified row last char is part of a multi line comment
* that starts at this row or at one before, and does not end at the end
* of the row but spawns to the next row. */
int editorRowHasOpenComment(erow *row) {
   if (row->hl && row->rsize && row->hl[row->rsize-1] == HL_MLCOMMENT &&
       (row->rsize < 2 || (row->render[row->rsize-2] != '*' ||
                           row->render[row->rsize-1] != '/'))) return 1;
   return 0;
}

/* Set every byte of row->hl (that corresponds to every character in the line)
* to the right syntax highlight type (HL_* defines). */
void editorUpdateSyntax(erow *row) {
   row->hl = realloc(row->hl,row->rsize);
   memset(row->hl,HL_NORMAL,row->rsize);
//highlights row->hl to HL_NORMAL 
   if (E.syntax == NULL) return; /* No syntax, everything is HL_NORMAL. */
   int i, prev_sep, in_string, in_comment;
   char *p;
   char **keywords = E.syntax->keywords;
   char *scs = E.syntax->singleline_comment_start;
   char *mcs = E.syntax->multiline_comment_start;
   char *mce = E.syntax->multiline_comment_end;
   /* Point to the first non-space char. */
   p = row->render;
   i = 0; /* Current char offset */
   while(*p && isspace(*p)) {
       p++;
       i++;
   }
   prev_sep = 1; /* Tell the parser if 'i' points to start of word. */
   in_string = 0; /* Are we inside "" or '' ? */
   in_comment = 0; /* Are we inside multi-line comment? */
   /* If the previous line has an open comment, this line starts
    * with an open comment state. */
   if (row->idx > 0 && editorRowHasOpenComment(&E.row[row->idx-1]))
       in_comment = 1;
   while(*p) {
       /* Handle // comments. */
       if (prev_sep && *p == scs[0] && *(p+1) == scs[1]) {
           /* From here to end is a comment */
           memset(row->hl+i,HL_COMMENT,row->size-i);
           return;
       }
       /* Handle multi line comments. */
       if (in_comment) {
           row->hl[i] = HL_MLCOMMENT;
           if (*p == mce[0] && *(p+1) == mce[1]) {
               row->hl[i+1] = HL_MLCOMMENT;
               p += 2; i += 2;
               in_comment = 0;
               prev_sep = 1;
               continue;
           } else {
               prev_sep = 0;
               p++; i++;
               continue;
           }
       } else if (*p == mcs[0] && *(p+1) == mcs[1]) {
           row->hl[i] = HL_MLCOMMENT;
           row->hl[i+1] = HL_MLCOMMENT;
           p += 2; i += 2;
           in_comment = 1;
           prev_sep = 0;
           continue;
       }

       /* Handle "" and '' */
       if (in_string) {
           row->hl[i] = HL_STRING;
           if (*p == '\\') {
               row->hl[i+1] = HL_STRING;
               p += 2; i += 2;
               prev_sep = 0;
               continue;
           }
           if (*p == in_string) in_string = 0;
           p++; i++;
           continue;
       } else {
           if (*p == '"' || *p == '\'') {
               in_string = *p;
               row->hl[i] = HL_STRING;
               p++; i++;
               prev_sep = 0;
               continue;
           }
       }
       /* Handle non printable chars. */
       if (!isprint(*p)) {
           row->hl[i] = HL_NONPRINT;
           p++; i++;
           prev_sep = 0;
           continue;
       }
       /* Handle numbers */
       if ((isdigit(*p) && (prev_sep || row->hl[i-1] == HL_NUMBER)) ||
           (*p == '.' && i >0 && row->hl[i-1] == HL_NUMBER)) { /* Include decimal points*/
           row->hl[i] = HL_NUMBER;
           p++; i++;
           prev_sep = 0;
           continue;
       }

       /* Handle keywords and lib calls */

       if (prev_sep) {
           int j;
           for (j = 0; keywords[j]; j++) {
               int klen = strlen(keywords[j]);
               int kw2 = keywords[j][klen-1] == '|';
               if (kw2) klen--;
               if (!memcmp(p,keywords[j],klen) &&
                   is_separator(*(p+klen)))
               {
                  /* Keyword */
                   memset(row->hl+i,kw2 ? HL_KEYWORD2 : HL_KEYWORD1,klen);
                   p += klen;
                   i += klen;
                   break;
               }
           }
           if (keywords[j] != NULL) {
               prev_sep = 0;
               continue; /* We had a keyword match */
           }
       }

       /* Not special chars */
       prev_sep = is_separator(*p);
       p++; i++;
   }
   /* Propagate syntax change to the next row if the open commen
    * state changed. This may recursively affect all the following rows
    * in the file. */
   int oc = editorRowHasOpenComment(row);
   if (row->hl_oc != oc && row->idx+1 < E.numrows)
       editorUpdateSyntax(&E.row[row->idx+1]);
     //reevaluates if the opencomment thing is changed; could potentially put the entire text in comments, so it is good to be recursive
   row->hl_oc = oc;
}
/* Maps syntax highlight token types to terminal colors. */

int editorSyntaxToColor(int hl) {
    switch(hl) {
   case HL_COMMENT:
   case HL_MLCOMMENT: return 36;     /* cyan */
   case HL_KEYWORD1: return 33;    /* yellow */
   case HL_KEYWORD2: return 32;    /* green */
   case HL_STRING: return 35;      /* magenta */
   case HL_NUMBER: return 31;      /* red */
   case HL_MATCH: return 34;      /* blue */
   default: return 37;             /* white */
   }
}

/* Select the syntax highlight scheme depending on the filename,
* setting it in the global state E.syntax. */

void editorSelectSyntaxHighlight(char *filename) {
   for (unsigned int j = 0; j < HLDB_ENTRIES; j++) {
       struct editorSyntax *s = HLDB+j;
       unsigned int i = 0;
       while(s->filematch[i]) {
           char *p;
           int patlen = strlen(s->filematch[i]);
           if ((p = strstr(filename,s->filematch[i])) != NULL) {
               if (s->filematch[i][0] != '.' || p[patlen] == '\0') {
                   E.syntax = s;
                   return;
               }
           }
           i++;
       }
   }
}

/* ======================= Editor rows implementation ======================= */
/* Update the rendered version and the syntax highlight of a row. */

void editorUpdateRow(erow *row) {
//update RENDER array when the text of the row changes
   int tabs = 0, nonprint = 0, j, idx;
  /* Create a version of the row we can directly print on the screen,
    * respecting tabs, substituting non printable characters with '?'. */
   free(row->render);
   for (j = 0; j < row->size; j++)
       if (row->chars[j] == TAB) tabs++;
   row->render = malloc(row->size + tabs*8 + nonprint*9 + 1);
   idx = 0;
   for (j = 0; j < row->size; j++) {
       if (row->chars[j] == TAB) {
           row->render[idx++] = ' ';
           while((idx+1) % 8 != 0) row->render[idx++] = ' ';
       } else {
           row->render[idx++] = row->chars[j];
       }
   }
   row->rsize = idx;
   row->render[idx] = '\0';
   /* Update the syntax highlighting attributes of the row. */
   editorUpdateSyntax(row);
}
/* Insert a row at the specified position, shifting the other rows on the bottom
* if required. */
void editorInsertRow(int at, char *s, size_t len) {
   if (at > E.numrows) return;
   E.row = realloc(E.row,sizeof(erow)*(E.numrows+1));
   if (at != E.numrows) {
       memmove(E.row+at+1,E.row+at,sizeof(E.row[0])*(E.numrows-at));
       for (int j = at+1; j <= E.numrows; j++) E.row[j].idx++;
   }
   E.row[at].size = len;
   E.row[at].chars = malloc(len+1);
   memcpy(E.row[at].chars,s,len+1);
   E.row[at].hl = NULL;
   E.row[at].bd = NULL;
   E.row[at].hl_oc = 0;
   E.row[at].render = NULL;
   E.row[at].rsize = 0;
   E.row[at].idx = at;
   editorUpdateRow(E.row+at);
   E.numrows++;
   E.dirty++;
}

/* Free row's heap allocated stuff. */
void editorFreeRow(erow *row) {
   free(row->render);
   free(row->chars);
   free(row->hl);
   free(row->bd);
}
/* Remove the row at the specified position, shifting the remainign on the
* top. */
void editorDelRow(int at) {
   erow *row;
   if (at >= E.numrows) return;
   row = E.row+at;
   editorFreeRow(row);
   memmove(E.row+at,E.row+at+1,sizeof(E.row[0])*(E.numrows-at-1));
   for (int j = at; j < E.numrows-1; j++) E.row[j].idx++;
   E.numrows--;   
   E.dirty++;
}
/* Turn the editor rows into a single heap-allocated string.
* Returns the pointer to the heap-allocated string and populate the
* integer pointed by 'buflen' with the size of the string, escluding
* the final nulterm. */
char *editorRowsToString(int *buflen) {
   char *buf = NULL, *p;
   int totlen = 0;
   int j;
   /* Compute count of bytes */
   for (j = 0; j < E.numrows; j++)
       totlen += E.row[j].size+1; /* +1 is for "\n" at end of every row */
   *buflen = totlen;
   totlen++; /* Also make space for nulterm */
   p = buf = malloc(totlen);
   for (j = 0; j < E.numrows; j++) {
       memcpy(p,E.row[j].chars,E.row[j].size);
       p += E.row[j].size;
       *p = '\n';
       p++;
   }
   *p = '\0';
   return buf;
}
/* Insert a character at the specified position in a row, moving the remaining
* chars on the right if needed. */
void editorRowInsertChar(erow *row, int at, int c) {

   if (at > row->size) {
       /* Pad the string with spaces if the insert location is outside the
        * current length by more than a single character. */
       int padlen = at-row->size;
       /* In the next line +2 means: new char and null term. */
       row->chars = realloc(row->chars,row->size+padlen+2);
       memset(row->chars+row->size,' ',padlen);
       row->chars[row->size+padlen+1] = '\0';
       row->size += padlen+1;
   } else {
       /* If we are in the middle of the string just make space for 1 new
        * char plus the (already existing) null term. */
       row->chars = realloc(row->chars,row->size+2);
       memmove(row->chars+at+1,row->chars+at,row->size-at+1);
       row->size++;
   }
   row->chars[at] = c;
   editorUpdateRow(row);
   
   row->bd = realloc(row->bd, (row->size + 1) * 4 );

   for(int i = row->size-1; i > at; i--){
      row->bd[i] = row->bd[i-1];
   }
   // Shifts Bold characters after at to match

   row->bd[at] = E.bolded;
  // Sets new character to bolded state

   E.dirty++;
}
/* Append the string 's' at the end of a row */
void editorRowAppendString(erow *row, char *s, int *b, size_t len) {


   row->chars = realloc(row->chars,row->size+len+1);


   memcpy(row->chars+row->size,s,len);

   row->bd = realloc(row->bd,(row->size+len + 1) * 4);
   for(int i = 0; i < len; i++){
    row->bd[row->size + i] = b[i];
   }

   row->size += len;


   row->chars[row->size] = '\0';


   editorUpdateRow(row);


   E.dirty++;


}

void boldToggle(){
  if(E.bolded){
    E.bolded = 0;
  }
  else{
    E.bolded = 1;
  }
  return;
}
// Toggles Bold state on and off


void copy(){  
  int len=0;
   for(int i=0; i<=(E.row[E.cy].chars[E.cx]);i++){
    if(!isspace(E.row[E.cy].chars[E.cx+i]) && E.row[E.cy].chars[E.cx+i]!='\0' && isprint(E.row[E.cy].chars[E.cx+i]))
      len++;
    else break;
  }

  char *str;
  str = (char *) malloc(len);
  int *b;
  b = (int *) malloc(len * 4);
  if(len<=0){
    editorSetStatusMessage("JEV: you can't copy that!");
  }
  else if(len>0){
  for(int i=0; i<=(E.row[E.cy].chars[E.cx]);i++){
    if(!isspace(E.row[E.cy].chars[E.cx+i]) && E.row[E.cy].chars[E.cx+i]!='\0'){
    //str=realloc(str,len);
    str[i]=E.row[E.cy].chars[E.cx+i];

  }
    else break;
  }
  E.copied_char_buffer=realloc(E.copied_char_buffer, strlen(str));
  E.copied_bd_buffer=realloc(E.copied_bd_buffer, strlen(str) * 4);
  strcpy(E.copied_char_buffer, str);
  for(int i = 0; i < strlen(str) / 8; i++){
    //E.copied_bd_buffer[i] = E.row[E.cy].bd[i];
    E.copied_bd_buffer[i] = 0;

  }
  editorSetStatusMessage("JEV: Text copied");
}

free(str);
free(b);
}

void copyRow(){
E.copied_char_buffer=realloc(E.copied_char_buffer, strlen(E.row[E.cy].chars)+1);
E.copied_bd_buffer=realloc(E.copied_bd_buffer, E.row[E.cy].size * 4);
strcpy(E.copied_char_buffer, E.row[E.cy].chars);
for(int i = 0; i < E.row[E.cy].size + 1; i++){
  E.copied_bd_buffer[i] = E.row[E.cy].bd[i];
}
editorSetStatusMessage("JEV: Text copied");
}

void paste(){
if(!E.copied_char_buffer) return;
if(E.cy==E.numrows)
editorInsertRow(E.cy, E.copied_char_buffer, strlen(E.copied_char_buffer));
else
editorRowAppendString(&E.row[E.cy],E.copied_char_buffer, E.copied_bd_buffer, strlen(E.copied_char_buffer));
E.cx+=strlen(E.copied_char_buffer);
}


/* Delete the character at offset 'at' from the specified row. */
void editorRowDelChar(erow *row, int at) {
   if (row->size <= at) return;
   memmove(row->chars+at,row->chars+at+1,row->size-at);
   editorUpdateRow(row);
   row->size--;
   for(int i = at; i < row->size; i++){
     row->bd[i] = row->bd[at + 1];
   }
   E.dirty++;
}

/* Insert the specified char at the current prompt position. */
void editorInsertChar(int c) {
   int filerow = E.rowoff+E.cy;
   int filecol = E.coloff+E.cx;
   erow *row = (filerow >= E.numrows) ? NULL : &E.row[filerow];
   /* If the row where the cursor is currently located does not exist in our
    * logical representaion of the file, add enough empty rows as needed. */
   if (!row) {
       while(E.numrows <= filerow)
           editorInsertRow(E.numrows,"",0);
   }
   row = &E.row[filerow];
   editorRowInsertChar(row,filecol,c);
   if (E.cx == E.screencols-1)
       E.coloff++;
   else
       E.cx++;
   E.dirty++;
}

/* Inserting a newline is slightly complex as we have to handle inserting a
* newline in the middle of a line, splitting the line as needed. */
void editorInsertNewline(void) {
   int filerow = E.rowoff+E.cy;
   int filecol = E.coloff+E.cx;
   erow *row = (filerow >= E.numrows) ? NULL : &E.row[filerow];
   if (!row) {
       if (filerow == E.numrows) {
           editorInsertRow(filerow,"",0);
           goto fixcursor;
       }
       return;
   }
   /* If the cursor is over the current line size, we want to conceptually
    * think it's just over the last character. */
   if (filecol >= row->size) filecol = row->size;
   if (filecol == 0) {
       editorInsertRow(filerow,"",0);
   } else {
       /* We are in the middle of a line. Split it between two rows. */
       editorInsertRow(filerow+1,row->chars+filecol,row->size-filecol);
       row = &E.row[filerow];
       row->chars[filecol] = '\0';
       row->size = filecol;
       editorUpdateRow(row);
   }
fixcursor:
   if (E.cy == E.screenrows-1) {
       E.rowoff++;
   } else {
       E.cy++;
   }
   E.cx = 0;
   E.coloff = 0;
}

/* Delete the char at the current prompt position. */
void editorDelChar() {
   int filerow = E.rowoff+E.cy;
   int filecol = E.coloff+E.cx;
   erow *row = (filerow >= E.numrows) ? NULL : &E.row[filerow];

   if (!row || (filecol == 0 && filerow == 0)) return;
   if (filecol == 0) {
       /* Handle the case of column 0, we need to move the current line
        * on the right of the previous one. */
       filecol = E.row[filerow-1].size;
       editorRowAppendString(&E.row[filerow-1],row->chars, row->bd, row->size);
       editorDelRow(filerow);
       row = NULL;
       if (E.cy == 0)
           E.rowoff--;
       else
           E.cy--;
       E.cx = filecol;
       if (E.cx >= E.screencols) {
           int shift = (E.screencols-E.cx)+1;
           E.cx -= shift;
           E.coloff += shift;
       }
   } else {
       editorRowDelChar(row,filecol-1);
       if (E.cx == 0 && E.coloff)
           E.coloff--;
       else
           E.cx--;
   }
   if (row) editorUpdateRow(row);
   E.dirty++;
}
/* Load the specified program in the editor memory and returns 0 on success
* or 1 on error. */
int editorOpen(char *filename) {
   FILE *fp;
   E.dirty = 0;
  free(E.filename);
  E.filename = strdup(filename);
   fp = fopen(filename,"r");
   if (!fp) {
       if (errno != ENOENT) {
           perror("Opening file");
           exit(1);
       }
       return 1;
   }

   char *line = NULL;
   size_t linecap = 0;
   ssize_t linelen;
   while((linelen = getline(&line,&linecap,fp)) != -1) {
       if (linelen && (line[linelen-1] == '\n' || line[linelen-1] == '\r'))
           line[--linelen] = '\0';
       editorInsertRow(E.numrows,line,linelen);
   }
   free(line);
   fclose(fp);
   E.dirty = 0;
   return 0;
}

/* Save the current file on disk. Return 0 on success, 1 on error. */
int editorSave(void) {
   int len;
   char *buf = editorRowsToString(&len);
   int fd = open(E.filename,O_RDWR|O_CREAT,0644);
   if (fd == -1) goto writeerr;
   /* Use truncate + a single write(2) call in order to make saving
    * a bit safer, under the limits of what we can do in a small editor. */
   if (ftruncate(fd,len) == -1) goto writeerr;
   if (write(fd,buf,len) != len) goto writeerr;
   close(fd);
   free(buf);
   E.dirty = 0;
   editorSetStatusMessage("JEV: %d BYTES WERE WRITTEN TO THE DISK", len);
   return 0;
   writeerr:
   free(buf);
   if (fd != -1) close(fd);
   editorSetStatusMessage("JEV: BRO WE CAN’T SAVE THAT! ur giving me this error dude: %s",strerror(errno));
   return 1;
}

/* ============================= Terminal update ============================ */
/* We define a very simple "append buffer" structure, that is an heap
* allocated string where we can append to. This is useful in order to
* write all the escape sequences in a buffer and flush them to the standard
* output in a single call, to avoid flickering effects. */
struct abuf {
   char *b;
   int len;
};
//adds to a buffer rather than being individual write calls, append the buffer increasing length and adding new contents 
#define ABUF_INIT {NULL,0}

void abAppend(struct abuf *ab, const char *s, int len) {
   char *new = realloc(ab->b,ab->len+len);
   if (new == NULL) return;
   memcpy(new+ab->len,s,len);
   ab->b = new;
   ab->len += len;
}
//allocates space for the additions to the buffer, uses memcpy to copy the new string/list of chars to the end of the buffer, increases length etc
void abFree(struct abuf *ab) {
   free(ab->b);
}
/* This function writes the whole screen using VT100 escape characters
 starting from the logical state of the editor in the global state 'E'. */
void editorRefreshScreen(void) {
   int y;
   erow *r;
   char buf[32];
   struct abuf ab = ABUF_INIT;
   abAppend(&ab,"\x1b[7m",4); //reverses colors of text editor 
   abAppend(&ab,"\x1b[?25l",6); /* Hide cursor. */
   abAppend(&ab,"\x1b[H",3); /* Go home. */
   for (y = 0; y < E.screenrows; y++) {
       int filerow = E.rowoff+y;
       if (filerow >= E.numrows) {
           if (E.numrows == 0 && y == E.screenrows/3) {
               char welcome[80];
               int welcomelen = snprintf(welcome,sizeof(welcome),
                  "Jesse Eden Vish for all you text editor needs. You can call me %s\x1b[0K\r\n", NAME);
               int padding = (E.screencols-welcomelen)/2;
//padding is the space between the number of extra cols around the size of the welcome message divided by 2. This centers ir 
               if (padding) {
                   abAppend(&ab,"|",1);
//puts | appended to buffer
                   padding--;
               }
               while(padding--) abAppend(&ab," ",1);
               abAppend(&ab,welcome,welcomelen);
           } else {
               abAppend(&ab,"|\x1b[0K\r\n",7);
           }
           continue;
       }
       r = &E.row[filerow];
       int len = r->rsize - E.coloff;
       int current_color = -1;
       int current_bold = -1;
       if (len > 0) {
           if (len > E.screencols) len = E.screencols;
//if length of welcome is longer than columns, make it equal to cols
           char *c = r->render+E.coloff;
           unsigned char *hl = r->hl+E.coloff;
           int *bd = r->bd+E.coloff;
           int j;
           for (j = 0; j < len; j++) {
               if (j == 0){
                 abAppend(&ab,"\x1b[0m", 4);
                 abAppend(&ab,"\x1b[7m", 4);
                 if(editorRowHasOpenComment(r)){
                  abAppend(&ab,"\x1b[36m", 5);
                 }
               }
               // resets graphics

               if (hl[j] == HL_NONPRINT) {
                   char sym;
                   abAppend(&ab,"\x1b[7m",4);
                   if (c[j] <= 26)
                       sym = '@'+c[j];
                   else
                       sym = '?';
                   abAppend(&ab,&sym,1);
                   abAppend(&ab,"\x1b[0m",4);
               } 
               else {
                  if (current_bold == -1 && r->bd[j]){
                     abAppend(&ab,"\x1b[1m", 4);
                     current_bold = 1;
                  }
                  else if (current_bold == 1 && !r->bd[j]){
                     abAppend(&ab,"\x1b[0m", 4);
                     abAppend(&ab,"\x1b[7m",4);
                     current_bold = -1; 
                  }
                  // Format Bold font at beginning of changes to bd

                  if (hl[j] == HL_NORMAL) {
                   if (current_color != -1) {
                       abAppend(&ab,"\x1b[39m",5);
//sets the default color with 39
                       current_color = -1;
                   }
                   abAppend(&ab,c+j,1);
               } else {
                   int color = editorSyntaxToColor(hl[j]);
                   if (color != current_color) {
                       char buf[16];
                       int clen = snprintf(buf,sizeof(buf),"\x1b[%dm",color);
                       current_color = color;
                       abAppend(&ab,buf,clen);
                   }
                   abAppend(&ab,c+j,1);
               }
             }
           }
       }
       abAppend(&ab,"\x1b[39m",5);
       abAppend(&ab,"\x1b[0K",4);
       abAppend(&ab,"\r\n",2);
   }

   /* Create a two rows status. First row: */
   abAppend(&ab,"\x1b[0K",4);
   abAppend(&ab,"\x1b[7m",4);
   char status[80], rstatus[80];
   int len = snprintf(status, sizeof(status), "%.20s - %d lines ",
       E.filename, E.numrows, E.dirty ? "(modified)": "");
   int rlen = snprintf(rstatus, sizeof(rstatus),
       "%d/%d",E.rowoff+E.cy+1,E.numrows);
   if (len > E.screencols) len = E.screencols;
   abAppend(&ab,status,len);
   while(len < E.screencols) {
       if (E.screencols - len == rlen) {
           abAppend(&ab,rstatus,rlen);
           break;
       } else {
           abAppend(&ab," ",1);
           len++;
       }
   }
   abAppend(&ab,"\x1b[0m\r\n",6);

   /* Second row depends on E.statusmsg and the status message update time. */
   abAppend(&ab,"\x1b[0K",4);
   int msglen = strlen(E.statusmsg);
   if (msglen && time(NULL)-E.statusmsg_time < 5)
       abAppend(&ab,E.statusmsg,msglen <= E.screencols ? msglen : E.screencols);

   /* Put cursor at its current position. Note that the horizontal position
    * at which the cursor is displayed may be different compared to 'E.cx'
    * because of TABs. */
   int j;
   int cx = 1;
   int filerow = E.rowoff+E.cy;
   erow *row = (filerow >= E.numrows) ? NULL : &E.row[filerow];
   if (row) {
       for (j = E.coloff; j < (E.cx+E.coloff); j++) {
           if (j < row->size && row->chars[j] == TAB) cx += 7-((cx)%8);
           cx++;
       }
   }
   snprintf(buf,sizeof(buf),"\x1b[%d;%dH",E.cy+1,cx);
   abAppend(&ab,buf,strlen(buf));
   abAppend(&ab,"\x1b[?25h",6); /* Show cursor. */
   write(STDOUT_FILENO,ab.b,ab.len);
   abFree(&ab);
}

/* Set an editor status message for the second line of the status, at the
* end of the screen. */
void editorSetStatusMessage(const char *fmt, ...) {
   va_list ap;
   va_start(ap,fmt);
   vsnprintf(E.statusmsg,sizeof(E.statusmsg),fmt,ap);
   va_end(ap);
   E.statusmsg_time = time(NULL);
}
/* =============================== Find mode ================================ */

#define JEV_QUERY_LEN 256

void editorFind(int fd) {
   char query[JEV_QUERY_LEN+1] = {0};
   int qlen = 0;
   int last_match = -1; /* Last line where a match was found. -1 for none. */
   int find_next = 0; /* if 1 search next, if -1 search prev. */
   int saved_hl_line = -1;  /* No saved HL */
//shows which LINE needs to be restored to original HL
   char *saved_hl = NULL;
//saves the original HL value to be restored after a search was done (goes back to white from blue)



#define FIND_RESTORE_HL do { \
   if (saved_hl) { \
       memcpy(E.row[saved_hl_line].hl,saved_hl, E.row[saved_hl_line].rsize); \
       saved_hl = NULL; \
   } \
} while (0)

   /* Save the cursor position in order to restore it later. */
   int saved_cx = E.cx, saved_cy = E.cy;
   int saved_coloff = E.coloff, saved_rowoff = E.rowoff;

   while(1) {
       editorSetStatusMessage(
           "JEV: Search: %s (Use ESC/Arrows/Enter)", query);
       editorRefreshScreen();
       int c = editorReadKey(fd);
       if (c == DEL_KEY || c == CTRL_H || c == BACKSPACE) {
           if (qlen != 0) query[--qlen] = '\0';
           last_match = -1;
       } else if (c == ESC || c == ENTER) {
           if (c == ESC) {
               E.cx = saved_cx; E.cy = saved_cy;
               E.coloff = saved_coloff; E.rowoff = saved_rowoff;
           }
           FIND_RESTORE_HL;
           editorSetStatusMessage("");
           return;
       } else if (c == ARROW_RIGHT || c == ARROW_DOWN) {
           find_next = 1;
       } else if (c == ARROW_LEFT || c == ARROW_UP) {
           find_next = -1;
       } else if (isprint(c)) {
           if (qlen < JEV_QUERY_LEN) {
               query[qlen++] = c;
               query[qlen] = '\0';
               last_match = -1;
           }
       }

       /* Search occurrence. */
       if (last_match == -1) find_next = 1;
       if (find_next) {
           char *match = NULL;
           int match_offset = 0;
           int i, current = last_match;


           for (i = 0; i < E.numrows; i++) {
               current += find_next;
               if (current == -1) current = E.numrows-1;
               else if (current == E.numrows) current = 0;
               match = strstr(E.row[current].render,query);
//checks if query is a substring of a given row 

               if (match) {
                   match_offset = match-E.row[current].render;
                   break;
               }
           }
           find_next = 0;

           /* Highlight */
           FIND_RESTORE_HL;

           if (match) {
               erow *row = &E.row[current];
               last_match = current;
               if (row->hl) {
                   saved_hl_line = current;
                   saved_hl = malloc(row->rsize);
                   memcpy(saved_hl,row->hl,row->rsize);
                   memset(row->hl+match_offset,HL_MATCH,qlen);
               }
               E.cy = 0;
               E.cx = match_offset;
               E.rowoff = current;
               E.coloff = 0;
               /* Scroll horizontally as needed. */
               if (E.cx > E.screencols) {
                   int diff = E.cx - E.screencols;
                   E.cx -= diff;
                   E.coloff += diff;
               }
           }
       }
   }


}

/* ========================= Editor events handling  ======================== */
/* Handle cursor position change because arrow keys were pressed. */


void editorMoveCursor(int key) {
   int filerow = E.rowoff+E.cy;
   int filecol = E.coloff+E.cx;
   int rowlen;
   erow *row = (filerow >= E.numrows) ? NULL : &E.row[filerow];

   switch(key) {
   case ARROW_LEFT:
       if (E.cx == 0) {
           if (E.coloff) {
               E.coloff--;
           } else {

               if (filerow > 0) {
                   E.cy--;
                   E.cx = E.row[filerow-1].size;
                   if (E.cx > E.screencols-1) {
                       E.coloff = E.cx-E.screencols+1;
                       E.cx = E.screencols-1;
                   }
               }
           }
       } else {
           E.cx -= 1;
       }
       break;
   case ARROW_RIGHT:
       if (row && filecol < row->size) {
           if (E.cx == E.screencols-1) {
               E.coloff++;
           } else {
               E.cx += 1;
           }
       } else if (row && filecol == row->size) {
           E.cx = 0;
           E.coloff = 0;
           if (E.cy == E.screenrows-1) {
               E.rowoff++;
           } else {
               E.cy += 1;
           }
       }
       break;
   case ARROW_UP:
       if (E.cy == 0) {
           if (E.rowoff) E.rowoff--;
       } else {
           E.cy -= 1;
       }
       break;
   case ARROW_DOWN:
       if (filerow < E.numrows) {
           if (E.cy == E.screenrows-1) {
               E.rowoff++;
           } else {
               E.cy += 1;
           }
       }
       break;
   //move cursor position with the up, down, left, right keys; also prevents the cursor from going off the screen
}
   /* Fix cx if the current line has not enough chars. */
   filerow = E.rowoff+E.cy;
   filecol = E.coloff+E.cx;
   row = (filerow >= E.numrows) ? NULL : &E.row[filerow];
   rowlen = row ? row->size : 0;
   if (filecol > rowlen) {
       E.cx -= filecol-rowlen;
       if (E.cx < 0) {
           E.coloff += E.cx;
           E.cx = 0;
       }
   }
}

/* Process events arriving from the standard input, which is, the user
* is typing stuff on the terminal. */
#define JEV_QUIT_TIMES 3
//how many more times you gotta press ctrl q to quit unsaved
void editorProcessKeypress(int fd) {
//processes the keys pressed, do different things based on which keys are pressed; maps keys to editor functions 
   /* When the file is modified, requires Ctrl-q to be pressed N times
    * before actually quitting. */
   static int quit_times = JEV_QUIT_TIMES;
   int c = editorReadKey(fd);

   switch(c) {
   case ENTER:         /* Enter */
       editorInsertNewline();
       break;
   case CTRL_B:
      boldToggle();
      break;
   case CTRL_C:        /* Ctrl-c */
       /* We ignore ctrl-c, it can't be so simple to lose the changes
        * to the edited file. */
       copy();
       break;
  case CTRL_K:
      copyRow();
      break;
  case CTRL_V:
      paste();
      break;
   case CTRL_Q:        /* Ctrl-q */
       /* Quit if the file was already saved. */
       if (E.dirty && quit_times) {
           editorSetStatusMessage("JEV: editorSetStatusMessage CHILL OUT!! This file has unsaved changes sir…. "
               "Press Ctrl-Q %d more times to quit.", quit_times);
           quit_times--;
           return;
       }
       clearScreen();
       exit(0);
       break;
   case CTRL_S:        /* Ctrl-s */
       editorSave();
       break;
   case CTRL_F:
       editorFind(fd);
       break;
   case BACKSPACE:     /* Backspace */
   case CTRL_H:        /* Ctrl-h */
   case DEL_KEY:
       editorDelChar();
       break;
   case PAGE_UP:
   case PAGE_DOWN:
       if (c == PAGE_UP && E.cy != 0)
           E.cy = 0;
       else if (c == PAGE_DOWN && E.cy != E.screenrows-1)
           E.cy = E.screenrows-1;
       {
       int times = E.screenrows;
       while(times--)
           editorMoveCursor(c == PAGE_UP ? ARROW_UP:
                                           ARROW_DOWN);
       }
       break;
   case ARROW_UP:
   case ARROW_DOWN:
   case ARROW_LEFT:
   case ARROW_RIGHT:
       editorMoveCursor(c);
       break;
   case CTRL_L: /* ctrl+l, clear screen */
       /* Just refresh the line as side effect. */
       break;
   case ESC:
       /* Nothing to do for ESC in this mode. */
       break;
   default:
       editorInsertChar(c);
       break;
   }
   quit_times = JEV_QUIT_TIMES; /* Reset it to the original value. */
}

int editorFileWasModified(void) {
   return E.dirty;
}


void initEditor(void) {
   E.cx = 0;
   E.cy = 0;
//horizontal and vertical location of cursor (top left) 
   E.rowoff = 0;
   E.coloff = 0;
   E.numrows = 0;
   E.row = NULL;
   E.dirty = 0;
   E.bolded = 0;
   E.filename = NULL;
   E.syntax = NULL;
//when E.syntax is null, no syntax hightlighting is to be done; default is no highlights like on txt files etc
   if (getWindowSize(STDIN_FILENO,STDOUT_FILENO,
                     &E.screenrows,&E.screencols) == -1)
   {
       perror("JEV: Unable to query the screen for size (columns / rows)");
       exit(1);
   }
   E.screenrows -= 2; /* Get room for status bar. */
}

int main(int argc, char **argv) {

   if (argc != 2) { //if theres not just one thing after ./JEV, print error
       fprintf(stderr,"JEV: Usage: JEV <filename>\n");
       exit(1);
   }
   initEditor();
   editorSelectSyntaxHighlight(argv[1]);
   editorOpen(argv[1]);
   enableRawMode(STDIN_FILENO);
   editorSetStatusMessage("JEV: HELP: Ctrl-S = save | Ctrl-Q = quit | Ctrl-F = find");
   while(1) {
       editorRefreshScreen();
       editorProcessKeypress(STDIN_FILENO);
   }
   
   return 0;
}
