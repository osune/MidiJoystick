#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>
#include <jack/ringbuffer.h>

#include "midijack.h"
#include "glue.h"
	   

jack_ringbuffer_t *jrb = NULL;

int _setup_jack(void);
int _setup_jack(void) { return setup_jack(&jrb, DEFAULT_RB_SIZE); }


/**
 * http://www.metasyntax.net/lang/scheme/gambit.html
 */

#define ___BEGIN_CFUN_SEND_MIDI(src,i) \
if ((___err = SEND_MIDI (src, i)) == ___FIX(___NO_ERR)) {
#define ___END_CFUN_SEND_MIDI(src,i) \
     ; }

#define ___BEGIN_SFUN_SEND_MIDI(src,dst)		\
{ ___err = SEND_MIDI (src,  ___RETURN_POS);
#define ___END_SFUN_SEND_MIDI(src) }


//int _send_midi(uint8_t *msg, size_t size) {
int SEND_MIDI(___SCMOBJ msg, int size) {

     struct midi_msg midi;

     ___SCMOBJ lst = msg;

     if (size == 0)
	  return 0;
     
     memset(&midi, 0, sizeof(midi));
     for (int i=0; i < size && ___PAIRP(lst) && i < MAX_MIDI_MSG; ++i) {
	  ___SCMOBJ scm_int = ___CAR(lst);
	  uint8_t c_int;
	  ___SCMOBJ ___err;
	  ___err = ___EXT(___SCMOBJ_to_U8) (scm_int, &c_int, size);
	  if (___err != ___FIX(___NO_ERR)) {
            return ___err;
	  }
	  midi.msg[i] = c_int;
	  lst = ___CDR(lst);
     }
     midi.size = (size_t) size;
//     fprintf(stderr, "m1: %x m2: %x m3: %x\n", midi.msg[0], midi.msg[1], midi.msg[2]);



     if (jack_ringbuffer_write_space(jrb) < sizeof(midi)) {
//	  fprintf(stderr, "midi message will not fit in ringbuffer\n");
	  return -1;
     }

     if(jack_ringbuffer_write(jrb, (void*) &midi, sizeof(midi)) != sizeof(midi))
	  return -1;
     return 0;
}
