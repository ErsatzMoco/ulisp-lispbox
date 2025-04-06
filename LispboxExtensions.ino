/*
  LispBox uLisp Extension - Version 1.0 - June 2024
  Hartmut Grawe - github.com/ersatzmoco - June 2024

  Based on:
  NeoPixel uLisp Extension - Version 1a - 22nd May 2023
  See http://www.ulisp.com/show?4GMV

  Adopted search-str from
  M5Cardputer editor version by hasn0life - Nov 2024 - https://github.com/hasn0life/ulisp-sedit-m5cardputer

  Licensed under the MIT license: https://opensource.org/licenses/MIT
*/

// #define radiohead // Outcomment this to switch to LowPowerLab RFM69 library
                     // CURRENTLY MANDATORY BECAUSE OF UNSOLVED ISSUES WITHIN RADIOHEAD LIBRARY --- USE LOWPOWERLAB LIBRARY FOR NOW.

#include <Adafruit_NeoPixel.h>
#if defined (RA8875_gfx)
  #include <Wire.h>
  #include <Adafruit_TSC2007.h>
#endif
#if defined oled_gfx
  #include <Wire.h>
  #include <U8g2lib.h>
#endif
#if defined rfm69
  #include <RFM69.h>
#endif
#if defined servolib
  #include <Servo.h>
#endif
#if defined matrixlib
  #include <Wire.h>
  #include <Adafruit_GFX.h>
  #include <Adafruit_LEDBackpack.h>
#endif

// #define NEOPIXEL_NUM 1      //uncomment these two lines when using external NeoPixels - fill in appropriate values
// #define PIN_NEOPIXEL 11

#if defined (RA8875_gfx)
  Adafruit_TSC2007 touch;
#endif

#if defined(oled_gfx)
  U8G2_SH1106_128X64_NONAME_F_HW_I2C oled = U8G2_SH1106_128X64_NONAME_F_HW_I2C(U8G2_R0, /* reset=*/U8X8_PIN_NONE);
#endif

#if defined(rfm69)
    // #define FREQUENCY RF69_868MHZ
  #define FREQUENCY RF69_433MHZ
  #define ENCRYPTKEY "My@@@Encrypt@@@@" //exactly the same 16 characters/bytes on all nodes!
  #define IS_RFM69HCW true // set to 'true' only if you are using an RFM69HCW module like on Feather M0 Radio
  #define RFM69_CS 37
  #define RFM69_IRQ 36
  #define RFM69_IRQN 36
  #define RFM69_RST 35
#endif


#if defined NEOPIXEL_NUM
  Adafruit_NeoPixel pixels(NEOPIXEL_NUM, PIN_NEOPIXEL, NEO_GRB + NEO_KHZ800);
#endif

#if defined(rfm69)
    #define PACKETLENGTH 65
    uint8_t pctlen = PACKETLENGTH+1;          //store pctlen globally for access via RadioHead library -- gets changed according to received byte packets
    char *packet = (char*)malloc(PACKETLENGTH+1);    //reserve global buffer memory for send and receive once -- PACKETLENGTH char bytes plus \0
    RFM69 radio(RFM69_CS, RFM69_IRQ, IS_RFM69HCW, RFM69_IRQN);
#endif

#if defined servolib
    struct ulservo {
      int snum;
      int pin;
      struct ulservo* nextservo;
      Servo servo;
    };

    struct ulservo* servolist = NULL;
    struct ulservo* curservo = NULL;
#endif

#if defined matrixlib
    Adafruit_8x16matrix matrix = Adafruit_8x16matrix();
#endif


/*
  Helper function: Read BGR-Pixel from BMP and convert it to RGB 565 (16 bit) color value
*/
uint16_t readBGR(File file) {
  int b = file.read();
  int g = file.read();
  int r = file.read();

  return ((r & 0xF8) << 8) | ((g & 0xFC) << 3) | (b >> 3);
} 

/*
  (set-backlight level)
  Set backlight level of standard GFX display.
*/
#if defined(gfxsupport)
object *fn_SetBacklight (object *args, object *env) {
  (void) env;
  uint8_t level = checkinteger(first(args));
  analogWrite(PIN_TFT_BACKLIGHT, level);
  return nil;
}

/*
  (display-bmp fname x y)
  Open BMP file fname from SD if it exits and display it on screen at position x y.
*/
object *fn_DisplayBMP (object *args, object *env) {
  (void) env;

  SD.begin(SDCARD_SS_PIN);

  int slength = stringlength(checkstring(first(args)))+1;
  char *fnbuf = (char*)malloc(slength);
  cstring(first(args), fnbuf, slength);
  File file;

  if (!SD.exists(fnbuf)) {
    pfstring("File not found", (pfun_t)pserial);
    free(fnbuf);
    return nil;
  }
  int x = checkinteger(second(args));
  int y = checkinteger(third(args));
  (void) args;

  char buffer[BUFFERSIZE];
  file = SD.open(fnbuf);
  if (!file) { 
    pfstring("Problem reading from SD card", (pfun_t)pserial);
    free(fnbuf);
    return nil;
  }

  char b = file.read();
  char m = file.read();
  if ((m != 77) || (b != 66)) {
    pfstring("No BMP file", (pfun_t)pserial);
    free(fnbuf);
    return nil;
  }

  file.seek(10);
  uint32_t offset = SDRead32(file);
  SDRead32(file);
  int32_t width = SDRead32(file);
  int32_t height = SDRead32(file);
  int zpad = 0;
  if ((width % 4) > 0) {
    zpad = (4 - ((width * 3) % 4));
  }

  file.seek(offset);

  tft.startWrite();
  int starttime;
  for (int ly = (y + height - 1); ly >= y; ly--) {
    for (int lx = x; lx < (x + width); lx++) {
      tft.writePixel(lx, ly, readBGR(file));
      //starttime = micros();
      //while (micros() < (starttime + 64));
    }
    //ignore trailing zero bytes
    if (zpad > 0) {
      for (int i = 0; i < zpad; i++) {
        file.read();
      }
    }
  }
  tft.endWrite();

  file.close();
  free(fnbuf);
  return nil;
}

/*
  (load-bmp fname arr [offx] [offy])
  Open RGB BMP file fname from SD if it exits and copy it into the two-dimensional uLisp array provided.
  Note that this allocates massive amounts of RAM; use for small bitmaps/icons only.
  When the image is larger than the array, only the upper leftmost area of the bitmap fitting into the array is loaded.
  Providing offx and offy you may move the "window" of the array to other parts of the bitmap (useful e.g. for tiling).
*/
object *fn_LoadBMP (object *args, object *env) {

  SD.begin(SDCARD_SS_PIN);

  int slength = stringlength(checkstring(first(args)))+1;
  char* fnbuf = (char*)malloc(slength);
  cstring(first(args), fnbuf, slength);
  File file;

  if (!SD.exists(fnbuf)) {
    pfstring("File not found", (pfun_t)pserial);
    free(fnbuf);
    return nil;
  }
  object* array = second(args);
  if (!arrayp(array)) {
    pfstring("Argument is not an array", (pfun_t)pserial);
    free(fnbuf);
    return nil;
  }
  object* dimensions = cddr(array);
  if (listp(dimensions)) {
    if (listlength(dimensions) != 2) {
      pfstring("Array must be two-dimensional", (pfun_t)pserial);
      free(fnbuf);
      return nil;
    }
  }
  else {
    pfstring("Array must be two-dimensional", (pfun_t)pserial);
      free(fnbuf);
      return nil;
  }
  args = cddr(args);
  int offx = 0;
  int offy = 0;

  if (args != NULL) {
    offx = checkinteger(car(args));
    args = cdr(args);
    if (args != NULL) {
      offy = checkinteger(car(args));
    }
  }
  
  (void) args;

  int aw = first(dimensions)->integer;
  int ah = second(dimensions)->integer;
  int bit;
  object* subscripts;
  object* ox;
  object* oy;
  object* oyy;
  object** element;

  char buffer[BUFFERSIZE];
  file = SD.open(fnbuf);
  if (!file) { 
    pfstring("Problem reading from SD card", (pfun_t)pserial);
    free(fnbuf);
    return nil;
  }

  char b = file.read();
  char m = file.read();
  if ((m != 77) || (b != 66)) {
    pfstring("No BMP file", (pfun_t)pserial);
    free(fnbuf);
    return nil;
  }

  file.seek(10);
  uint32_t offset = SDRead32(file);
  SDRead32(file);
  int32_t width = SDRead32(file);
  int32_t height = SDRead32(file);
  int zpad = 0;
  if ((width % 4) > 0) {
    zpad = (4 - ((width * 3) % 4));
  }

  file.seek(offset);

  for (int ly = (height - 1); ly >= 0; ly--) {
    for (int lx = 0; lx < width; lx++) {
      if ((lx < (aw+offx)) && (ly < (ah+offy)) && (lx >= offx) && (ly >= offy)) {
        ox = number(lx-offx);
        oy = number(ly-offy);
        oyy = cons(oy, NULL);
        subscripts = cons(ox, oyy);
        element = getarray(array, subscripts, env, &bit);
        *element = number(readBGR(file));

        myfree(subscripts);
        myfree(oyy);
        myfree(oy);
        myfree(ox);
      }
      else {
        file.read();
        file.read();
        file.read();
      }
    }
    //ignore trailing zero bytes
    if (zpad > 0) {
      for (int i = 0; i < zpad; i++) {
        file.read();
      }
    }
  }

  file.close();
  free(fnbuf);
  return nil;
}

/*
  (load-mono fname arr [offx] [offy])
  Open monochrome BMP file fname from SD if it exits and copy it into the two-dimensional uLisp bit array provided.
  When the image is larger than the array, only the upper leftmost area of the bitmap fitting into the array is loaded.
  Providing offx and offy you may move the "window" of the array to other parts of the bitmap (useful e.g. for tiling).
*/
object *fn_LoadMono (object *args, object *env) {

  SD.begin(SDCARD_SS_PIN);

  int slength = stringlength(checkstring(first(args)))+1;
  char* fnbuf = (char*)malloc(slength);
  cstring(first(args), fnbuf, slength);
  File file;

  if (!SD.exists(fnbuf)) {
    pfstring("File not found", (pfun_t)pserial);
    free(fnbuf);
    return nil;
  }
  object* array = second(args);
  if (!arrayp(array)) {
    pfstring("Argument is not an array", (pfun_t)pserial);
    free(fnbuf);
    return nil;
  }
  object* dimensions = cddr(array);
  if (listp(dimensions)) {
    if (listlength(dimensions) != 2) {
      pfstring("Array must be two-dimensional", (pfun_t)pserial);
      free(fnbuf);
      return nil;
    }
  }
  else {
    pfstring("Array must be two-dimensional", (pfun_t)pserial);
      free(fnbuf);
      return nil;
  }
  args = cddr(args);
  int offx = 0;
  int offy = 0;

  if (args != NULL) {
    offx = checkinteger(car(args));
    args = cdr(args);
    if (args != NULL) {
      offy = checkinteger(car(args));
    }
  }
  (void) args;

  int aw = abs(first(dimensions)->integer);
  int ah = abs(second(dimensions)->integer);
  int bit;
  object* subscripts;
  object* ox;
  object* oy;
  object* oyy;
  object** element;

  char buffer[BUFFERSIZE];
  file = SD.open(fnbuf);
  if (!file) { 
    pfstring("Problem reading from SD card", (pfun_t)pserial);
    free(fnbuf);
    return nil;
  }

  char b = file.read();
  char m = file.read();
  if ((m != 77) || (b != 66)) {
    pfstring("No BMP file", (pfun_t)pserial);
    free(fnbuf);
    return nil;
  }

  file.seek(10);
  uint32_t offset = SDRead32(file);
  SDRead32(file);
  int32_t width = SDRead32(file);
  int32_t height = SDRead32(file);
  int linebytes = floor(width / 8);
  int restbits = width % 8;
  if (restbits > 0) linebytes++;
  int zpad = 0;
  if ((linebytes % 4) > 0) {
    zpad = (4 - (linebytes % 4));
  }

  file.seek(28);
  uint16_t depth = file.read();
  if (depth > 1) { 
    pfstring("No monochrome bitmap file", (pfun_t)pserial);
    free(fnbuf);
    return nil;
  }

  file.seek(offset);

  int lx = 0;
  int bmpbyte = 0;
  int bmpbit = 0;

  for (int ly = (height - 1); ly >= 0; ly--) {
    for (int bx = 0; bx < linebytes; bx++) {
      bmpbyte = file.read();
      for (int bix = 0; bix < 8; bix++) {
        lx = (bx * 8) + bix;
        if ((lx < (aw+offx)) && (ly < (ah+offy)) && (lx >= offx) && (ly >= offy)) {
          ox = number(lx-offx);
          oy = number(ly-offy);
          oyy = cons(oy, NULL);
          subscripts = cons(ox, oyy);
          element = getarray(array, subscripts, env, &bit);

          bmpbit = bmpbyte & (1 << (7-bix));
          if (bmpbit > 0) {
            bmpbit = 1;
          }
          else {
            bmpbit = 0;
          }
          *element = number((checkinteger(*element) & ~(1<<bit)) | bmpbit<<bit);

          myfree(subscripts);
          myfree(oyy);
          myfree(oy);
          myfree(ox);
        }
      }
    }
    //ignore trailing zero bytes
    if (zpad > 0) {
      for (int i = 0; i < zpad; i++) {
        file.read();
      }
    }
  }

  file.close();
  free(fnbuf);
  return nil;
}

/*
  (show-bmp arr x y [monocol])
  Show bitmap image contained in uLisp array arr on screen at position x y.
  The function automatically distinguishes between monochrome and color image arrays.
  If monocol is provided, a monochrome image is painted with that color value.
*/
object *fn_ShowBMP (object *args, object *env) {

  object* array = first(args);
  if (!arrayp(array)) error2("argument is not an array");

  object *dimensions = cddr(array);
  if (listp(dimensions)) {
    if (listlength(dimensions) != 2) error2("array must be two-dimensional");
  }
  else error2("array must be two-dimensional");

  int x = checkinteger(second(args));
  int y = checkinteger(third(args));

  int monocol = 0xFFFF;
  args = cddr(args);
  args = cdr(args);
  if (args != NULL) {
    monocol = checkinteger(first(args));
  }
  (void) args;

  int aw = abs(first(dimensions)->integer);
  int ah = abs(second(dimensions)->integer);
  int bit;
  object* subscripts;
  object* ox;
  object* oy;
  object* oyy;
  object** element;

  tft.startWrite();
  int starttime;
  int bmpbit = 0;
  uint16_t color = 0;
  for (int ay = 0; ay < ah; ay++) {
    for (int ax = 0; ax < aw; ax++) {
      ox = number(ax);
      oy = number(ay);
      oyy = cons(oy, NULL);
      subscripts = cons(ox, oyy);
      element = getarray(array, subscripts, env, &bit);
      if (bit < 0) {
        tft.writePixel(x+ax, y+ay, checkinteger(*element));
      }
      else {
        bmpbit = abs(checkinteger(*element) & (1<<bit));
        if (bmpbit > 0) {
          color = monocol;
        }
        else {
          color = 0;
        }
        tft.writePixel(x+ax, y+ay, color);
      }
      //starttime = micros();
      //while (micros() < (starttime + 64));

      myfree(subscripts);
      myfree(oyy);
      myfree(oy);
      myfree(ox);
    }
  }
  tft.endWrite();

  return nil;
}
#endif


//USB host keyboard and mouse supported anytime
/*
  Helper function:
  Translate keys if necessary - separate from REPL key translation

*/
char translate_key (uint16_t temp, uint8_t mod) {
    #if defined(qwertz)
    unsigned char kout = 0;
    //Serial.print(mod); Serial.print(" "); Serial.println(temp);
    switch (temp) {
      case 96: kout = '^'; break;
      case 93: 
          if (mod == 0) {
              kout = '+';
          }
          else if (mod == 0x40) {
              kout = '~';
          }
          break;

      case 121: kout = 'z'; break;
      case 122: kout = 'y'; break;
      case 89: kout = 'Z'; break;
      case 90: kout = 'Y'; break;
      case 47: kout = '-'; break;
      case 92: kout = '#'; break;
      case 61: kout = '\''; break;

      case 39: kout = '('; break;
      case 91: kout = ')'; break;
      case 59: kout = '['; break;

      case 64:
          if (mod == 2) kout = '\"'; break;
  //    case 35:
  //        if (mod == 2) kout = 0xA7; break;
      case 94:
          if (mod == 2) kout = '&'; break;
      case 38:
          if (mod == 2) kout = '/'; break;
      case 42:
          if (mod == 2) kout = '('; break;
      case 40:
          if (mod == 2) kout = ')'; break;
      case 41:
          if (mod == 2) kout = '='; break;

      case 125:
          if (mod == 2) kout = '*'; break;

      case 60:
          if (mod == 2) kout = ';'; break;
      case 62:
          if (mod == 2) kout = ':'; break;
      case 63:
          if (mod == 2) kout = '_'; break;
      case 124:
          if (mod == 2) kout = '\''; break;
      case 95:
          if (mod == 2) kout = '?'; break;
      case 34:
          if (mod == 2) kout = '<'; break;
      case 123:
          if (mod == 2) kout = '>'; break;
      case 58:
          if (mod == 2) kout = ']'; break;

      case 55:
          if (mod == 0x40) {
            kout = '{';
          }
          else if (mod == 4) {
            kout = 247;
          }
          else {
            kout = temp;
          } 
          break;
      case 56:
          if (mod == 0x40) {
            kout = '[';
          }
          else if (mod == 4) {
            kout = 248;
          }
          else {
            kout = temp;
          } 
          break;
      case 57:
          if (mod == 0x40) {
            kout = ']';
          }
          else if (mod == 4) {
            kout = 249;
          }
          else {
            kout = temp;
          } 
          break;
      case 48:
          if (mod == 0x40) {
            kout = '}';
          }
          else if (mod == 4) {
            kout = 240;
          }
          else {
            kout = temp;
          } 
          break;

      case 113:
          if (mod == 0x40) {
            kout = '@';
          }
          else {
            kout = temp;
          }
          break;

      case 45:
          if (mod == 0x40) kout = '\\'; break;

      case 99:
          if (mod == 4) {
            kout = 251;
          }
          else {
            kout = temp;
          } 
          break;

      case 118:
          if (mod == 4) {
            kout = 252;
          }
          else {
            kout = temp;
          } 
          break;

      case 120:
          if (mod == 4) {
            kout = 253;
            }
          else {
            kout = temp;
          } 
          break;

      case 49:
          if (mod == 4) {
            kout = 241;
          }
          else {
            kout = temp;
          } 
          break;

      case 50:
          if (mod == 4) {
            kout = 242;
          }
          else {
            kout = temp;
          } 
          break;

      case 51:
          if (mod == 4) {
            kout = 243;
          }
          else {
            kout = temp;
          }
          break;

      case 52:
          if (mod == 4) {
            kout = 244;
          }
          else {
            kout = temp;
          } 
          break;

      case 53:
          if (mod == 4) {
            kout = 245;
          }
          else {
            kout = temp;
          }
          break;

      case 54:
          if (mod == 4) {
            kout = 246;
          }
          else {
            kout = temp;
          } 
          break;

      default: kout = temp; break;
    }
  #else
    kout = temp;
  #endif
  return kout;
}
 
/*
  (keyboard-get-key [pressed])
  Get key last recognized - default: when released, if [pressed] is t: when pressed).
*/
object *fn_KeyboardGetKey (object *args, object *env) {
  (void) env;
  bool check_press = false;
  if (args != NULL) {
    check_press = (first(args) == nil) ? false : true;
  }

  if (!check_press) {
    if (kb_released_k) {
      return number(translate_key(kb_released_k, kb_released_m));
    }
    else return nil;
  }
  else {
    if (kb_pressed_k) {
      return number(translate_key(kb_pressed_k, kb_pressed_m));
    }
    else return nil;
  }
}

/*
  (keyboard-flush)
  Discard missing key up/down events.
*/
object *fn_KeyboardFlush (object *args, object *env) {
  (void) args, (void) env;
  kb_pressed_k = 0;
  kb_pressed_m = 0;
  kb_released_k = 0;
  kb_released_m = 0;

  return nil;
}

/*
  (mouse-get-values)
  Query HID mouse interface and return values relx, rely, buttons, relwheel, relwheelH as a list.
*/
object *fn_MouseGetValues (object *args, object *env) {
  (void) args, (void) env;

  if (mouse.available()) {
    ms_buttons = mouse.getButtons();
    object *mb = number(ms_buttons);
    object *mx = number(mouse.getMouseX());
    object *my = number(mouse.getMouseY());
    object *mw = number(mouse.getWheel());
    object *mwh = number(mouse.getWheelH());

    mouse.mouseDataClear();

    return cons(mx, cons(my, cons(mb, cons(mw, cons(mwh, NULL)))));
  }
  else {
    return nil;
  }
}

/*
  (mouse-last-buttons)
  Returns last mouse button status without retrieving current values.
*/
object *fn_MouseLastButtons (object *args, object *env) {
  (void) args, (void) env;

  object *mb = number(ms_buttons);

  return mb;
}

/*
  (search-str pattern target [startpos])
  Returns the index of the first occurrence of pattern in target, or nil if it's not found starting from startpos.
*/
object *fn_searchstr (object *args, object *env) {
  (void) env;
  
  int startpos = 0;
  object *pattern = first(args);
  object *target = second(args);
  args = cddr(args);
  if (pattern == NULL) return number(0);
  else if (target == NULL) return nil;
  if (args != NULL) startpos = checkinteger(car(args));
  
if (stringp(pattern) && stringp(target)) {
    int l = stringlength(target);
    int m = stringlength(pattern);
    if (startpos > l) error2(indexrange);
    for (int i = startpos; i <= l-m; i++) {
      int j = 0;
      while (j < m && nthchar(target, i+j) == nthchar(pattern, j)) j++;
      if (j == m) return number(i);
    }
    return nil;
  } else error2("arguments are not both lists or strings");
  return nil;
}

/*
  (rad-to-deg n)
  Convert radians to degrees.
*/
object *fn_RadToDeg (object *args, object *env) {
  (void) env;

  return number(checkintfloat(first(args))*RAD_TO_DEG);
}

/*
  (deg-to-rad n)
  Convert degree to radians.
*/
object *fn_DegToRad (object *args, object *env) {
  (void) env;

  return number(checkintfloat(first(args))*DEG_TO_RAD);
}

/*
  (vector-sub v1 v2)
  Subtract vector v2 from vector v1 (lists with 3 elements).
*/
object *fn_VectorSub (object *args, object *env) {
  (void) env;

  if (!listp(first(args)) || !listp(second(args))) error2("arguments must be lists of three numbers");

  float a1 = checkintfloat(car(first(args)));
  float a2 = checkintfloat(car(cdr(first(args))));
  float a3 = checkintfloat(car(cddr(first(args))));

  float b1 = checkintfloat(car(second(args)));
  float b2 = checkintfloat(car(cdr(second(args))));
  float b3 = checkintfloat(car(cddr(second(args))));

  return cons(number(a1-b1), cons(number(a2-b2), cons(number(a3-b3), NULL)));
}

/*
  (vector-add v1 v2)
  Add vector v2 to vector v1 (lists with 3 elements).
*/
object *fn_VectorAdd (object *args, object *env) {
  (void) env;

  if (!listp(first(args)) || !listp(second(args))) error2("arguments must be lists of three numbers");

  float a1 = checkintfloat(car(first(args)));
  float a2 = checkintfloat(car(cdr(first(args))));
  float a3 = checkintfloat(car(cddr(first(args))));

  float b1 = checkintfloat(car(second(args)));
  float b2 = checkintfloat(car(cdr(second(args))));
  float b3 = checkintfloat(car(cddr(second(args))));

  return cons(number(a1+b1), cons(number(a2+b2), cons(number(a3+b3), NULL)));
}

/*
  (vector-norm v)
  Calculate magnitude/norm of vector v (list with 3 elements).
*/
object *fn_VectorNorm (object *args, object *env) {
  (void) env;

  if (!listp(first(args))) error2("argument must be list of three numbers");

  float a1 = checkintfloat(car(first(args)));
  float a2 = checkintfloat(car(cdr(first(args))));
  float a3 = checkintfloat(car(cddr(first(args))));

  return number(sqrt(a1*a1 + a2*a2 + a3*a3));
}

/*
  (scalar-mult v s)
  Multiply vector v (list with 3 elements) by number s (scalar).
*/
object *fn_ScalarMult (object *args, object *env) {
  (void) env;

  if (!listp(first(args))) error2("argument must be list of three numbers");

  float a1 = checkintfloat(car(first(args)));
  float a2 = checkintfloat(car(cdr(first(args))));
  float a3 = checkintfloat(car(cddr(first(args))));
  float s = checkintfloat(second(args));

  return cons(number(a1*s), cons(number(a2*s), cons(number(a3*s), NULL)));
}

/*
  (dot-product v1 v2)
  Calculate dot product of two three-dimensional vectors v1, v2 (lists with 3 elements).
*/
object *fn_DotProduct (object *args, object *env) {
  (void) env;

  if (!listp(first(args)) || !listp(second(args))) error2("arguments must be lists of three numbers");

  float a1 = checkintfloat(car(first(args)));
  float a2 = checkintfloat(car(cdr(first(args))));
  float a3 = checkintfloat(car(cddr(first(args))));

  float b1 = checkintfloat(car(second(args)));
  float b2 = checkintfloat(car(cdr(second(args))));
  float b3 = checkintfloat(car(cddr(second(args))));

  return number(a1*b1 + a2*b2 + a3*b3);
}

/*
  (cross-product v1 v2)
  Calculate cross product of two three-dimensional vectors v1, v2 (lists with 3 elements).
*/
object *fn_CrossProduct (object *args, object *env) {
  (void) env;

  if (!listp(first(args)) || !listp(second(args))) error2("arguments must be lists of three numbers");

  float a1 = checkintfloat(car(first(args)));
  float a2 = checkintfloat(car(cdr(first(args))));
  float a3 = checkintfloat(car(cddr(first(args))));

  float b1 = checkintfloat(car(second(args)));
  float b2 = checkintfloat(car(cdr(second(args))));
  float b3 = checkintfloat(car(cddr(second(args))));

  float c1 = a2*b3 - a3*b2;
  float c2 = a3*b1 - a1*b3;
  float c3 = a1*b2 - a2*b1;

  return cons(number(c1), cons(number(c2), cons(number(c3), NULL)));
}

/*
  (vector-angle v1 v2)
  Calculate angle (rad) between two three-dimensional vectors v1, v2 (lists with 3 elements).
*/
object *fn_VectorAngle (object *args, object *env) {
  (void) env;

  if (!listp(first(args)) || !listp(second(args))) error2("arguments must be lists of three numbers");

  float a1 = checkintfloat(car(first(args)));
  float a2 = checkintfloat(car(cdr(first(args))));
  float a3 = checkintfloat(car(cddr(first(args))));

  float b1 = checkintfloat(car(second(args)));
  float b2 = checkintfloat(car(cdr(second(args))));
  float b3 = checkintfloat(car(cddr(second(args))));

  //dot product
  float dot = (a1*b1 + a2*b2 + a3*b3);

  //norms
  float na = sqrt(a1*a1 + a2*a2 + a3*a3);
  float nb = sqrt(b1*b1 + b2*b2 + b3*b3);

  float cphi = dot/(na*nb);

  return number(acos(cphi));
}


#if defined sdcardsupport
/*
  (sd-file-exists filename)
  Returns t if filename exists on SD card, otherwise nil.
*/
object *fn_SDFileExists (object *args, object *env) {
  (void) args, (void) env;

  SD.begin(SDCARD_SS_PIN);

  int slength = stringlength(checkstring(first(args)))+1;
  char *fnbuf = (char*)malloc(slength);
  cstring(first(args), fnbuf, slength);

  if (SD.exists(fnbuf)) {
    free(fnbuf);
    return tee;
  }
  else {
    free(fnbuf);
    return nil;
  }
}

/*
  (sd-file-remove filename)
  Returns t if filename exists on SD card, otherwise nil.
*/
object *fn_SDFileRemove (object *args, object *env) {
  (void) args, (void) env;

  SD.begin(SDCARD_SS_PIN);
  int slength = stringlength(checkstring(first(args)))+1;
  char *fnbuf = (char*)malloc(slength);
  cstring(first(args), fnbuf, slength);

  if (SD.exists(fnbuf)) {
    SD.remove(fnbuf);
    free(fnbuf);
    return tee;
  }
  else {
    free(fnbuf);
    return nil;
  }
}

/*
  (sd-card-dir [mode])
  Print SD card directory and return nothing [mode 0, optional] or a uLisp string object [mode 1] or a List [mode 2].
*/
object *fn_SDCardDir (object *args, object *env) {
  (void) env;
  int mode = 0;
  if (args != NULL) {
    mode = checkinteger(first(args));
    if (mode > 2) mode = 2;
    if (mode < 0) mode = 0;
  }

  SD.begin(SDCARD_SS_PIN);
  File root = SD.open("/");
  int numTabs = 0;
  String dirstr;
  object* dirlist = NULL;

  if (mode == 0) {
    printDirectory(root, numTabs);
    return nil;
  }
  else if (mode == 1) {
    dirstr = printDirectoryStr(root, dirstr);
    return lispstring(dirstr.c_str());
  }
  else if (mode == 2) {
    dirlist = printDirectoryList(root);
    return dirlist;
  }
  else return nil;
}

//Directory functions (modified Arduino PD code)
String printDirectory(File dir, int numTabs) {
  pfun_t pf = pserial;

  while (true) {
    File entry =  dir.openNextFile();
    if (! entry) {
      // no more files
      break;
    }
    for (uint8_t i = 0; i < numTabs; i++) {
      pfstring("  ", pf);
    }
    pfstring(entry.name(), pf);
    if (entry.isDirectory()) {
      pfstring("/\n", pf);
      printDirectory(entry, numTabs + 1);
    } else {
      // files have sizes, directories do not
      pfstring(" ", pf);
      pint(entry.size(), pf);
      pfstring("\n", pf);
    }
    entry.close();
  }
  return NULL;
}

String printDirectoryStr(File dir, String dirstr) {
  while (true) {
    File entry =  dir.openNextFile();
    if (! entry) {
      // no more files
      break;
    }
    dirstr = dirstr + entry.name();
    if (entry.isDirectory()) {
      dirstr = dirstr + "/~%";
      dirstr = printDirectoryStr(entry, dirstr);
    } else {
      // files have sizes, directories do not
      dirstr = dirstr + " " + String(entry.size()) + "~%";
    }
    entry.close();
  }
  return dirstr;
}

object* printDirectoryList(File dir) {
  String dirstr = "";
  object* dirlist = NULL;

  while (true) {
    File entry =  dir.openNextFile();
    if (! entry) {
      // no more files
      break;
    }
    dirstr = entry.name();
    if (entry.isDirectory()) {
      dirstr = dirstr + "/";
      dirlist = cons(printDirectoryList(entry), dirlist);
      dirlist = cons(lispstring(dirstr.c_str()), dirlist);
    } else {
      dirlist = cons(lispstring(dirstr.c_str()), dirlist);
    }
    
    entry.close();
  }
  return dirlist;
}
#endif


#if defined NEOPIXEL_NUM
/*
  (pixels-begin)
  Configures the NeoPixel pin for output.
*/d
object *fn_PixelsBegin (object *args, object *env) {
  (void) args, (void) env;
  pixels.begin();
  return nil;
}

/*
  (pixels-clear)
  Sets all pixel colors to off.
*/
object *fn_PixelsClear (object *args, object *env) {
  (void) env;
  pixels.clear();
  return nil;
}

/*
  (pixels-fill [rgbw] [first] [fill])
  Fills all or part of the NeoPixel strip with a fixed 32-bit packed RGB or RGBW value (default 0).
  first, default 0, the first NeoPixel to fill.
  fill, default all, the number of NeoPixels to fill.
*/
object *fn_PixelsFill (object *args, object *env) {
  (void) env;
  uint32_t rgbw = 0;
  int first = 0, fill = 0;
  if (args != NULL) {
    first = checkinteger(first(args));
    args = cdr(args);
    if (args != NULL) {
      fill = checkinteger(first(args));
    }
  }
  pixels.fill(rgbw, first, fill);
  return nil;
}

/*
  (pixels-set-pixel-color index rgbw)
  (pixels-set-pixel-color index red green blue [white])
  Set a pixel's color using either a 32-bit packed RGB or RGBW value,
  or separate red, green, blue, and optionally white components.
*/
object *fn_PixelsSetPixelColor (object *args, object *env) {
  (void) env;
  int nargs = listlength(args);
  int i = checkinteger(first(args));
  if (nargs == 2) pixels.setPixelColor(i, checkinteger(second(args)));
  else {
    int w = 0;
    args = cdr(args);
    int r = checkinteger(first(args)), g = checkinteger(second(args)), b = checkinteger(third(args));
    args = cddr(cdr(args));
    if (args != NULL) w = checkinteger(first(args));
    pixels.setPixelColor(i, r, g, b, w);
  }
  return nil;
}

/*
  (pixels-color red green blue [white])
  Converts separate red, green, blue, and optionally white values into
  a single packed 32-bit RGB or RGBW color.
*/
object *fn_PixelsColor (object *args, object *env) {
  (void) env;
  int w = 0;
  int r = checkinteger(first(args)), g = checkinteger(second(args)), b = checkinteger(third(args));
  args = cddr(cdr(args));
  if (args != NULL) w = checkinteger(first(args));
  return number(pixels.Color(r, g, b, w));
}

/*
  (pixels-color-hsv hue sat val)
  Converts separate hue (0 to 65535), saturation (0 to 255), and value (0 to 255) values into
  a single packed 32-bit RGB or RGBW color.
*/
object *fn_PixelsColorHSV (object *args, object *env) {
  (void) env;
  int hue = checkinteger(first(args)), sat = checkinteger(second(args)), val = checkinteger(third(args));
  return number(pixels.ColorHSV(hue, sat, val));
}

/*
  (pixels-show)
  Transmits the pixel data to the NeoPixels.
*/
object *fn_PixelsShow (object *args, object *env) {
  (void) env;
  pixels.show();
  return nil;
}

/*
  (pixels-rainbow [first-hue] [cycles] [saturation] [brightness] [gammify])
  Fills the NeoPixel strip with one or more cycles of hues. 
  first-hue, default 0, is the hue of the first pixel (0 to 65535).
  cycles, default 1, is the number of cycles.
  saturation, default 255, is the saturation (0 to 255).
  brightness, default 255, is the brightness (0 to 255).
  gammify, default true, applies gamma correction to colours.
*/
object *fn_PixelsRainbow (object *args, object *env) {
  (void) env;
  int firstHue = 0, cycles = 1, saturation = 255, brightness = 255, gammify = 1;
  if (args != NULL) {
    firstHue = checkinteger(first(args));
    args = cdr(args);
    if (args != NULL) {
      cycles = checkinteger(first(args));
      args = cdr(args);
      if (args != NULL) {
        saturation = checkinteger(first(args));
        args = cdr(args);
        if (args != NULL) {
          brightness = checkinteger(first(args));
          args = cdr(args);
          if (args != NULL) {
            gammify = (first(args) == nil) ? false : true;
          }
        }
      }
    }
  }
  pixels.rainbow(firstHue, cycles, saturation, brightness, gammify);
  return nil;
}
#endif


#if defined (RA8875_gfx)
/*
  (tft1-begin)
  Initialize TFT
*/
object *fn_TFT1Begin (object *args, object *env) {
  (void) args, (void) env;

  if (!tft1.begin(RA8875_800x480)) {
    Serial.println("RA8875 Not Found!");
    return nil;
  }
  return tee;
}

/*
  (tft1-on)
  Switch TFT and backlight on and fill screen with black.
*/
object *fn_TFT1On (object *args, object *env) {
  (void) args, (void) env;

  tft1.displayOn(true);
  tft1.GPIOX(true);      // Enable TFT - display enable tied to GPIOX
  tft1.PWM1config(true, RA8875_PWM_CLK_DIV1024); // PWM output for backlight
  tft1.PWM1out(255);
  tft1.fillScreen(RA8875_BLACK);
  return nil;
}

/*
  (tft1-set-rotation)
  Set rotation of screen, see Adafruit GFX.
*/
object *fn_TFT1SetRotation (object *args, object *env) {
  (void) env;
  tft1.setRotation(checkinteger(first(args)));
  return nil;
}

/*
  (tft1-text-mode)
  Switch to text mode.
*/
object *fn_TFT1TextMode (object *args, object *env) {
  (void) args, (void) env;

  tft1.textMode();
  return nil;
}

/*
  (tft1-graphics-mode)
  Switch to graphics mode.
*/
object *fn_TFT1GraphicsMode (object *args, object *env) {
  (void) args, (void) env;

  tft1.graphicsMode();
  return nil;
}

/*
  (tft1-set-cursor x y)
  Set text cursor to location x (column) y (row).
*/
object *fn_TFT1SetCursor (object *args, object *env) {
  (void) env;

  tft1.textSetCursor(checkinteger(first(args)), checkinteger(second(args)));
  return nil;
}

/*
  (tft1-set-text-color fg bg)
  Set foreground (and optional background) color(s) for text display. Colors are a 565-coded 16-bit-value.
*/
object *fn_TFT1SetTextColor (object *args, object *env) {
  (void) env;
  if (cdr(args) != NULL) tft1.textColor((uint16_t)checkinteger(first(args)), (uint16_t)checkinteger(second(args)));
  else tft1.textColor((uint16_t)checkinteger(first(args)), 0);
  return nil;
}

/*
  (tft1-text-enlarge ef)
  Enlarge text (0 is standard size).
*/
object *fn_TFT1TextEnlarge (object *args, object *env) {
  (void) env;

  tft1.textEnlarge(checkinteger(first(args)));
  return nil;
}

/*
  (tft1-write-text str)
  Write string str to screen.
*/
object *fn_TFT1WriteText (object *args, object *env) {
  (void) env;

  int slength = stringlength(checkstring(first(args)))+1;
  char *tftbuf = (char*)malloc(slength);
  cstring(first(args), tftbuf, slength);
  tft1.textWrite(tftbuf);
  free(tftbuf);
  return nil;
}

/*
  (tft1-set-scroll-win x y width height mode)
  Set scroll window and scroll mode.
*/
object *fn_TFT1SetScrollWin (object *args, object *env) {
  (void) env;

  int x, y, width, height, mode = 0;

  x = checkinteger(first(args));
  y = checkinteger(second(args));
  args = cddr(args);
  width = checkinteger(first(args));
  height = checkinteger(second(args));
  args = cddr(args);
  if (args != NULL) mode = checkinteger(first(args)); 
  tft1.setScrollWindow(x, y, width, height, mode);
  return nil;
}

/*
  (tft1-set-scrolly lines)
  Scroll screen vertically lines pixel.
*/
object *fn_TFT1SetScrollY (object *args, object *env) {
  (void) env;

  int lines = 0;

  if (args != NULL) {
    lines = checkinteger(first(args));
    }
  tft1.scrollY(lines);
  return nil;
}

/*
  (tft1-set-scrollx cols)
  Scroll screen horizontally cols pixel.
*/
object *fn_TFT1SetScrollX (object *args, object *env) {
  (void) env;

  int cols = 0;

  if (args != NULL) {
    cols = checkinteger(first(args));
    }
  tft1.scrollX(cols);
  return nil;
}

/*
  (tft1-blink-cursor bool)
  If bool then switch on blinking cursor, else switch it off.
*/
object *fn_TFT1BlinkCursor (object *args, object *env) {
  (void) env;

  bool blink = false;

  if (args != NULL) {
    blink = (first(args) == nil) ? false : true;
    }
  if (blink) {
    tft1.cursorBlink(32);
  }
  else {
    tft1.writeCommand(RA8875_MWCR0);
    uint8_t temp = tft1.readData();
    temp &= 0x9F;
    tft1.writeData(temp);
  }
  return nil;
}

/*
  (tft1-draw-pixel)
  Wrapper copied from uLisp GFX. See doc there.
*/
object *fn_TFT1DrawPixel (object *args, object *env) {
  (void) env;
  uint16_t colour = COLOR_WHITE;
  if (cddr(args) != NULL) colour = checkinteger(third(args));
  tft1.drawPixel(checkinteger(first(args)), checkinteger(second(args)), colour);
  return nil;
}

/*
  (tft1-draw-line)
  Wrapper copied from uLisp GFX. See doc there.
*/
object *fn_TFT1DrawLine (object *args, object *env) {
  (void) env;
  uint16_t params[4], colour = COLOR_WHITE;
  for (int i=0; i<4; i++) { params[i] = checkinteger(car(args)); args = cdr(args); }
  if (args != NULL) colour = checkinteger(car(args));
  tft1.drawLine(params[0], params[1], params[2], params[3], colour);
  return nil;
}

/*
  (tft1-draw-rect)
  Wrapper copied from uLisp GFX. See doc there.
*/
object *fn_TFT1DrawRect (object *args, object *env) {
  (void) env;
  uint16_t params[4], colour = COLOR_WHITE;
  for (int i=0; i<4; i++) { params[i] = checkinteger(car(args)); args = cdr(args); }
  if (args != NULL) colour = checkinteger(car(args));
  tft1.drawRect(params[0], params[1], params[2], params[3], colour);
  return nil;
}

/*
  (tft1-fill-rect)
  Wrapper copied from uLisp GFX. See doc there.
*/
object *fn_TFT1FillRect (object *args, object *env) {
  (void) env;
  uint16_t params[4], colour = COLOR_WHITE;
  for (int i=0; i<4; i++) { params[i] = checkinteger(car(args)); args = cdr(args); }
  if (args != NULL) colour = checkinteger(car(args));
  tft1.fillRect(params[0], params[1], params[2], params[3], colour);
  return nil;
}

/*
  (tft1-draw-circle)
  Wrapper copied from uLisp GFX. See doc there.
*/
object *fn_TFT1DrawCircle (object *args, object *env) {
  (void) env;
  uint16_t params[3], colour = COLOR_WHITE;
  for (int i=0; i<3; i++) { params[i] = checkinteger(car(args)); args = cdr(args); }
  if (args != NULL) colour = checkinteger(car(args));
  tft1.drawCircle(params[0], params[1], params[2], colour);
  return nil;
}

/*
  (tft1-fill-circle)
  Wrapper copied from uLisp GFX. See doc there.
*/
object *fn_TFT1FillCircle (object *args, object *env) {
  (void) env;
  uint16_t params[3], colour = COLOR_WHITE;
  for (int i=0; i<3; i++) { params[i] = checkinteger(car(args)); args = cdr(args); }
  if (args != NULL) colour = checkinteger(car(args));
  tft1.fillCircle(params[0], params[1], params[2], colour);
  return nil;
}

/*
  (tft1-draw-ellipse)
  Draw ellipse at position x y with long arm and short arm.
*/
object *fn_TFT1DrawEllipse (object *args, object *env) {
  (void) env;
  uint16_t params[4], colour = COLOR_WHITE;
  for (int i=0; i<4; i++) { params[i] = checkinteger(car(args)); args = cdr(args); }
  if (args != NULL) colour = checkinteger(car(args));
  tft1.drawEllipse(params[0], params[1], params[2], params[3], colour);
  return nil;
}

/*
  (tft1-fill-ellipse)
  Fill ellipse at position x y with long arm and short arm.
*/
object *fn_TFT1FillEllipse (object *args, object *env) {
  (void) env;
  uint16_t params[4], colour = COLOR_WHITE;
  for (int i=0; i<4; i++) { params[i] = checkinteger(car(args)); args = cdr(args); }
  if (args != NULL) colour = checkinteger(car(args));
  tft1.fillEllipse(params[0], params[1], params[2], params[3], colour);
  return nil;
}

/*
  (tft1-draw-round-rect)
  Wrapper copied from uLisp GFX. See doc there.
*/
object *fn_TFT1DrawRoundRect (object *args, object *env) {
  (void) env;
  uint16_t params[5], colour = COLOR_WHITE;
  for (int i=0; i<5; i++) { params[i] = checkinteger(car(args)); args = cdr(args); }
  if (args != NULL) colour = checkinteger(car(args));
  tft1.drawRoundRect(params[0], params[1], params[2], params[3], params[4], colour);
  return nil;
}

/*
  (tft1-fill-round-rect)
  Wrapper copied from uLisp GFX. See doc there.
*/
object *fn_TFT1FillRoundRect (object *args, object *env) {
  (void) env;
  uint16_t params[5], colour = COLOR_WHITE;
  for (int i=0; i<5; i++) { params[i] = checkinteger(car(args)); args = cdr(args); }
  if (args != NULL) colour = checkinteger(car(args));
  tft1.fillRoundRect(params[0], params[1], params[2], params[3], params[4], colour);
  return nil;
}

/*
  (tft1-draw-triangle)
  Wrapper copied from uLisp GFX. See doc there.
*/
object *fn_TFT1DrawTriangle (object *args, object *env) {
  (void) env;
  uint16_t params[6], colour = COLOR_WHITE;
  for (int i=0; i<6; i++) { params[i] = checkinteger(car(args)); args = cdr(args); }
  if (args != NULL) colour = checkinteger(car(args));
  tft1.drawTriangle(params[0], params[1], params[2], params[3], params[4], params[5], colour);
  return nil;
}

/*
  (tft1-fill-triangle)
  Wrapper copied from uLisp GFX. See doc there.
*/
object *fn_TFT1FillTriangle (object *args, object *env) {
  (void) env;
  uint16_t params[6], colour = COLOR_WHITE;
  for (int i=0; i<6; i++) { params[i] = checkinteger(car(args)); args = cdr(args); }
  if (args != NULL) colour = checkinteger(car(args));
  tft1.fillTriangle(params[0], params[1], params[2], params[3], params[4], params[5], colour);
  return nil;
}

/*
  (tft1-draw-curve x y long short curve)
  Draw curve at position x y with long arm, short arm and curve part.
*/
object *fn_TFT1DrawCurve (object *args, object *env) {
  (void) env;
  uint16_t params[5], colour = COLOR_WHITE;
  for (int i=0; i<5; i++) { params[i] = checkinteger(car(args)); args = cdr(args); }
  if (args != NULL) colour = checkinteger(car(args));
  tft1.drawCurve(params[0], params[1], params[2], params[3], params[4], colour);
  return nil;
}

/*
  (tft1-fill-curve x y long short curve)
  Fill curve at position x y with long arm, short arm and curve part.
*/
object *fn_TFT1FillCurve (object *args, object *env) {
  (void) env;
  uint16_t params[5], colour = COLOR_WHITE;
  for (int i=0; i<5; i++) { params[i] = checkinteger(car(args)); args = cdr(args); }
  if (args != NULL) colour = checkinteger(car(args));
  tft1.fillCurve(params[0], params[1], params[2], params[3], params[4], colour);
  return nil;
}

/*
  (tft1-fill-screen)
  Wrapper copied from uLisp GFX. See doc there.
*/
object *fn_TFT1FillScreen (object *args, object *env) {
  (void) env;
  uint16_t colour = COLOR_BLACK;
  if (args != NULL) colour = checkinteger(first(args));
  tft1.fillScreen(colour);
  return nil;
}

/*
  (tft1-display-bmp fname x y)
  Opens file fname from SD if it exits and displays it on screen at position x y.
*/
object *fn_TFT1DisplayBMP (object *args, object *env) {
  (void) env;

  SD.begin(SDCARD_SS_PIN);

  int slength = stringlength(checkstring(first(args)))+1;
  char *fnbuf = (char*)malloc(slength);
  cstring(first(args), fnbuf, slength);
  File file;

  if (!SD.exists(fnbuf)) {
    pfstring("File not found", (pfun_t)pserial);
    free(fnbuf);
    return nil;
  }
  int x = checkinteger(second(args));
  int y = checkinteger(third(args));
  (void) args;

  char buffer[BUFFERSIZE];
  file = SD.open(fnbuf);
  if (!file) { 
    pfstring("Problem reading from SD card", (pfun_t)pserial);
    free(fnbuf);
    return nil;
  }

  char b = file.read();
  char m = file.read();
  if ((m != 77) || (b != 66)) {
    pfstring("No BMP file", (pfun_t)pserial);
    free(fnbuf);
    return nil;
  }

  file.seek(10);
  uint32_t offset = SDRead32(file);
  SDRead32(file);
  int32_t width = SDRead32(file);
  int32_t height = SDRead32(file);
  int zpad = 0;
  if ((width % 4) > 0) {
    zpad = (4 - ((width * 3) % 4));
  }
  uint16_t linearr[width];

  file.seek(offset);

  tft1.graphicsMode();

  for (int ly = (y + height - 1); ly >= y; ly--) {
    for (int lx = 0; lx < width; lx++) {
      linearr[lx] = readBGR(file);
    }
    tft1.drawPixels(linearr, width, x, ly);
    //ignore trailing zero bytes
    if (zpad > 0) {
      for (int i = 0; i < zpad; i++) {
        file.read();
      }
    }
  }
  
  file.close();
  free(fnbuf);
  return nil;
}

/*
  (tft1-show-bmp arr x y [monocol])
  Show bitmap image contained in uLisp array arr on screen at position x y.
  The function automatically distinguishes between monochrome and color image arrays.
  If monocol is provided, a monochrome image is painted with that color value.
*/
object *fn_TFT1ShowBMP (object *args, object *env) {

  object* array = first(args);
  if (!arrayp(array)) error2("argument is not an array");

  object *dimensions = cddr(array);
  if (listp(dimensions)) {
    if (listlength(dimensions) != 2) error2("array must be two-dimensional");
  }
  else error2("array must be two-dimensional");

  int x = checkinteger(second(args));
  int y = checkinteger(third(args)); 
  int monocol = 0xFFFF;
  args = cddr(args);
  args = cdr(args);
  if (args != NULL) {
    monocol = checkinteger(first(args));
  } 
  (void) args;

  int bmpbit = 0;
  uint16_t color = 0;
  int aw = abs(first(dimensions)->integer);
  int ah = abs(second(dimensions)->integer);
  int bit;
  object* subscripts;
  object* ox;
  object* oy;
  object* oyy;
  object** element;

  uint16_t linearr[aw];
  tft1.graphicsMode();

  for (int ay = 0; ay < ah; ay++) {
    for (int ax = 0; ax < aw; ax++) {
      ox = number(ax);
      oy = number(ay);
      oyy = cons(oy, NULL);
      subscripts = cons(ox, oyy);
      element = getarray(array, subscripts, env, &bit);
      if (bit < 0) {
        linearr[ax] = checkinteger(*element);
      }
      else {
        bmpbit = abs(checkinteger(*element) & (1<<bit));
        if (bmpbit > 0) {
          color = monocol;
        }
        else {
          color = 0;
        }
        linearr[ax] = color;
      }
      myfree(subscripts);
      myfree(oyy);
      myfree(oy);
      myfree(ox);
    }
    tft1.drawPixels(linearr, aw, x, y+ay);
  }

  return nil;
}


/*
  (tft1-write-reg reg val)
  Low-level access: Write val to register reg.
*/
object *fn_TFT1WriteReg (object *args, object *env) {
  (void) env;
  tft1.writeReg(checkinteger(first(args)), checkinteger(second(args)));
  return nil;
}

/*
  (tft1-read-reg reg)
  Low-level access: Read register reg.
*/
object *fn_TFT1ReadReg (object *args, object *env) {
  (void) env;
  return number(tft1.readReg(checkinteger(first(args))));
}

/*
  (tft1-write-data d)
  Low-level access: Write data d to current register.
*/
object *fn_TFT1WriteData (object *args, object *env) {
  (void) env;
  tft1.writeData(checkinteger(first(args)));
  return nil;
}

/*
  (tft1-read-data)
  Low-level access: Read data value from current register.
*/
object *fn_TFT1ReadData (object *args, object *env) {
  (void) args, (void) env;
  return number(tft1.readData());
}

/*
  (tft1-write-command c)
  Low-level access: Write command c to current register.
*/
object *fn_TFT1WriteCommand (object *args, object *env) {
  (void) env;
  tft1.writeCommand(checkinteger(first(args)));
  return nil;
}

/*
  (tft1-read-status)
  Low-level access: Read status from current register.
*/
object *fn_TFT1ReadStatus (object *args, object *env) {
  (void) args, (void) env;
  return number(tft1.readStatus());
}

/*
  (tft1-set-backlight level)
  Set backlight to level [0-255].
*/
object *fn_TFT1SetBacklight (object *args, object *env) {
  (void) env;
  uint8_t level = constrain(checkinteger(first(args)), 0, 255);
  tft1.PWM1out(level);
  return nil;
}

//
// Touch screen routines for use with RA8875 and TSC2007 controller
// Replace "Wire2" and address in touch.begin according to your config if necessary
//

/*
  (touch-begin)
  Initialize touch screen controller.
*/
object *fn_TouchBegin (object *args, object *env) {
  (void) args, (void) env;
  if (touch.begin(0x48, &Wire2)) {
    return tee;
  }
  else {
    return nil;
  }
}

/*
  (touch-get-point)
  Return touched x,y position as a list.
*/
object *fn_TouchGetPoint (object *args, object *env) {
  (void) args, (void) env;

  uint16_t x, y, z1, z2;
  if (touch.read_touch(&x, &y, &z1, &z2)) {
    //int myx = map(p.y, TS_MINX, TS_MAXX, 0, tft.width());
    //int myy = map(p.x, TS_MINY, TS_MAXY, tft.height(), 0);
    int myx = x;
    int myy = y;

    object *px = number(myx);
    object *py = number(myy);
    object *pz1 = number(z1);
    object *pz2 = number(z2);

    return cons(px, cons(py, cons(pz1, cons(pz2, NULL))));
  }
  else {
    return nil;
  }
}


/*
  (touch-wait-for-touch)
  Wait for touch screen response and return touched x,y position as a list.
  Convenience function, not included in driver. (Blocking!)
*/
object *fn_TouchWaitForTouch (object *args, object *env) {
  (void) args, (void) env;

  uint16_t x, y, z1, z2;
  while (!touch.read_touch(&x, &y, &z1, &z2));
  //int myx = map(p.y, TS_MINX, TS_MAXX, 0, tft.width());
  //int myy = map(p.x, TS_MINY, TS_MAXY, tft.height(), 0);
  int myx = x;
  int myy = y;

  object *px = number(myx);
  object *py = number(myy);
  object *pz1 = number(z1);
  object *pz2 = number(z2);

  return cons(px, cons(py, cons(pz1, cons(pz2, NULL))));
}


#endif


//
// OLED graphics and text routines - in part a modified copy of GFX routines in uLisp core
// No stream support to avoid major modification of uLisp core 
//

#if defined (oled_gfx)
/*
  (oled-begin [adr])
  Initialize OLED (optionally using I2C address adr, otherwise using default address #x3C (7 bit)/#x78 (8 bit)).
*/
object *fn_OledBegin (object *args, object *env) {
  (void) env;

  uint8_t adr = 0x78;
  if (args != NULL) {
    adr = (uint8_t)checkinteger(first(args));
  }

  oled.setI2CAddress(adr);
  if (!oled.begin()) {
    Serial.println("OLED Not Found!");
    return nil;
  }
  oled.clearBuffer();
  oled.setFont(u8g2_font_profont12_mr);
  return tee;
}  

/*
  (oled-clear)
  Clear OLED.
*/
object *fn_OledClear (object *args, object *env) {
  (void) args, (void) env;

  oled.clearBuffer();
  oled.sendBuffer();
  return nil;
}

/*
  (oled-set-rotation rot)
  Set rotation of screen. 0 = no rotation, 1 = 90 degrees, 2 = 180 degrees, 3 = 270 degrees, 4 = hor. mirrored, 5 = vert. mirrored.
*/
object *fn_OledSetRotation (object *args, object *env) {
  (void) env;
  uint8_t rot = (uint8_t)checkinteger(first(args));
  if (rot > 5) rot = 5;

  switch (rot) {
    case 0:
        oled.setDisplayRotation(U8G2_R0);
    case 1:
        oled.setDisplayRotation(U8G2_R1);
    case 2:
        oled.setDisplayRotation(U8G2_R2);
    case 3:
        oled.setDisplayRotation(U8G2_R3);
    case 4:
        oled.setDisplayRotation(U8G2_MIRROR);
    case 5:
        oled.setDisplayRotation(U8G2_MIRROR_VERTICAL);    
  }
  
  oled.sendBuffer();
  return nil;
}

/*
  (oled-set-color fg)
  Set foreground color for text and graphics. Colors are 0 (clear/black), 1 (set/white) and 2 (XOR).
*/
object *fn_OledSetColor (object *args, object *env) {
  (void) env;

  uint8_t col = (uint8_t)checkinteger(first(args));
  if (col > 2) col = 2;

  oled.setDrawColor(col);
  return nil;
}

/*
  (oled-write-char x y c)
  Write char c to screen at location x y.
*/
object *fn_OledWriteChar (object *args, object *env) {
  (void) env;

  oled.drawGlyph(checkinteger(first(args)), checkinteger(second(args)), checkchar(third(args)));
  oled.sendBuffer();
  return nil;
}

/*
  (oled-write-string x y str)
  Write string str to screen at location x y.
*/
object *fn_OledWriteString (object *args, object *env) {
  (void) env;

  int slength = stringlength(checkstring(third(args)))+1;
  char *oledbuf = (char*)malloc(slength);
  cstring(third(args), oledbuf, slength);
  oled.drawStr(checkinteger(first(args)), checkinteger(second(args)), oledbuf);
  oled.sendBuffer();
  free(oledbuf);
  return nil;
}

/*
  (oled-draw-pixel x y)
  Draw pixel at position x y (using color set before).
*/
object *fn_OledDrawPixel (object *args, object *env) {
  (void) env;
  oled.drawPixel(checkinteger(first(args)), checkinteger(second(args)));
  oled.sendBuffer();
  return nil;
}

/*
  (oled-draw-line x0 y0 x1 y1)
  Draw a line between positions x0/y0 and x1/y1.
*/
object *fn_OledDrawLine (object *args, object *env) {
  (void) env;
  uint16_t params[4];
  for (int i=0; i<4; i++) { params[i] = checkinteger(car(args)); args = cdr(args); }
  oled.drawLine(params[0], params[1], params[2], params[3]);
  oled.sendBuffer();
  return nil;
}

/*
  (oled-draw-hline x y w)
  Draw fast horizontal line at position X Y with length w.
*/
object *fn_OledDrawHLine (object *args, object *env) {
  (void) env;
  uint16_t params[3];
  for (int i=0; i<3; i++) { params[i] = checkinteger(car(args)); args = cdr(args); }
  oled.drawHLine(params[0], params[1], params[2]);
  oled.sendBuffer();
  return nil;
}

/*
  (oled-draw-vline x y h)
  Draw fast vertical line at position X Y with length h.
*/
object *fn_OledDrawVLine (object *args, object *env) {
  (void) env;
  uint16_t params[3];
  for (int i=0; i<3; i++) { params[i] = checkinteger(car(args)); args = cdr(args); }
  oled.drawVLine(params[0], params[1], params[2]);
  oled.sendBuffer();
  return nil;
}

/*
  (oled-draw-rect x y w h)
  Draw empty rectangle at x y with width w and height h.
*/
object *fn_OledDrawRect (object *args, object *env) {
  (void) env;
  uint16_t params[4];
  for (int i=0; i<4; i++) { params[i] = checkinteger(car(args)); args = cdr(args); }
  oled.drawFrame(params[0], params[1], params[2], params[3]);
  oled.sendBuffer();
  return nil;
}

/*
  (oled-fill-rect x y w h)
  Draw filled rectangle at x y with width w and height h.
*/
object *fn_OledFillRect (object *args, object *env) {
  (void) env;
  uint16_t params[4];
  for (int i=0; i<4; i++) { params[i] = checkinteger(car(args)); args = cdr(args); }
  oled.drawBox(params[0], params[1], params[2], params[3]);
  oled.sendBuffer();
  return nil;
}

/*
  (oled-draw-circle x y r)
  Draw empty circle at position x y with radius r.
*/
object *fn_OledDrawCircle (object *args, object *env) {
  (void) env;
  uint16_t params[3];
  for (int i=0; i<3; i++) { params[i] = checkinteger(car(args)); args = cdr(args); }
  oled.drawCircle(params[0], params[1], params[2]);
  oled.sendBuffer();
  return nil;
}

/*
  (oled-fill-circle x y r)
  Draw filled circle at position x y with radius r.
*/
object *fn_OledFillCircle (object *args, object *env) {
  (void) env;
  uint16_t params[3];
  for (int i=0; i<3; i++) { params[i] = checkinteger(car(args)); args = cdr(args); }
  oled.drawDisc(params[0], params[1], params[2]);
  oled.sendBuffer();
  return nil;
}

/*
  (oled-draw-round-rect x y w h r)
  Draw empty rectangle at x y with width w and height h. Edges are rounded with radius r.
*/
object *fn_OledDrawRoundRect (object *args, object *env) {
  (void) env;
  uint16_t params[5];
  for (int i=0; i<5; i++) { params[i] = checkinteger(car(args)); args = cdr(args); }
  oled.drawRFrame(params[0], params[1], params[2], params[3], params[4]);
  oled.sendBuffer();
  return nil;
}

/*
  (oled-fill-round-rect)
  Draw filled rectangle at x y with width w and height h. Edges are rounded with radius r.
*/
object *fn_OledFillRoundRect (object *args, object *env) {
  (void) env;
  uint16_t params[5];
  for (int i=0; i<5; i++) { params[i] = checkinteger(car(args)); args = cdr(args); }
  oled.drawRBox(params[0], params[1], params[2], params[3], params[4]);
  oled.sendBuffer();
  return nil;
}

/*
  (oled-fill-triangle x0 y0 x1 y1 x2 y2)
  Draw filled triangle with corners at x0/y0, x1/y1 and x2/y2.
*/
object *fn_OledFillTriangle (object *args, object *env) {
  (void) env;
  uint16_t params[6];
  for (int i=0; i<6; i++) { params[i] = checkinteger(car(args)); args = cdr(args); }
  oled.drawTriangle(params[0], params[1], params[2], params[3], params[4], params[5]);
  oled.sendBuffer();
  return nil;
}

/*
  (oled-display-bmp fname x y)
  Open monochrome BMP file fname from SD if it exits and display it on screen at position x y (using the color set before).
*/
object *fn_OledDisplayBMP (object *args, object *env) {
  (void) env;

  SD.begin(SDCARD_SS_PIN);

  int slength = stringlength(checkstring(first(args)))+1;
  char *fnbuf = (char*)malloc(slength);
  cstring(first(args), fnbuf, slength);
  File file;

  if (!SD.exists(fnbuf)) {
    pfstring("File not found", (pfun_t)pserial);
    free(fnbuf);
    return nil;
  }
  int x = checkinteger(second(args));
  int y = checkinteger(third(args));
  (void) args;

  char buffer[BUFFERSIZE];
  file = SD.open(fnbuf);
  if (!file) { 
    pfstring("Problem reading from SD card", (pfun_t)pserial);
    free(fnbuf);
    return nil;
  }

  char b = file.read();
  char m = file.read();
  if ((m != 77) || (b != 66)) {
    pfstring("No BMP file", (pfun_t)pserial);
    free(fnbuf);
    return nil;
  }

  file.seek(10);
  uint32_t offset = SDRead32(file);
  SDRead32(file);
  int32_t width = SDRead32(file);
  int32_t height = SDRead32(file);
  int linebytes = floor(width / 8);
  int restbits = width % 8;
  if (restbits > 0) linebytes++;
  int zpad = 0;
  if ((linebytes % 4) > 0) {
    zpad = (4 - (linebytes % 4));
  }

  file.seek(28);
  uint16_t depth = file.read();
  if (depth > 1) { 
    pfstring("No monochrome bitmap file", (pfun_t)pserial);
    free(fnbuf);
    return nil;
  }

  file.seek(offset);

  int lx = 0;
  int bmpbyte = 0;
  int bmpbit = 0;

  for (int ly = (height - 1); ly >= 0; ly--) {
    for (int bx = 0; bx < linebytes; bx++) {
      bmpbyte = file.read();
      for (int bix = 0; bix < 8; bix++) {
        lx = (bx * 8) + bix;
        bmpbit = bmpbyte & (1 << (7-bix));
        if (bmpbit > 0) {
          oled.drawPixel(x+lx, y+ly);
        }
      }
    }
    //ignore trailing zero bytes
    if (zpad > 0) {
      for (int i = 0; i < zpad; i++) {
        file.read();
      }
    }
  }

  oled.sendBuffer();
  file.close();
  free(fnbuf);
  return nil;
}

/*
  (oled-show-bmp arr x y)
  Show bitmap image contained in uLisp array arr on screen at position x y (using color set before).
*/
object *fn_OledShowBMP (object *args, object *env) {

  object* array = first(args);
  if (!arrayp(array)) error2("argument is not an array");

  object *dimensions = cddr(array);
  if (listp(dimensions)) {
    if (listlength(dimensions) != 2) error2("array must be two-dimensional");
  }
  else error2("array must be two-dimensional");

  int x = checkinteger(second(args));
  int y = checkinteger(third(args));
  (void) args;

  int aw = abs(first(dimensions)->integer);
  int ah = abs(second(dimensions)->integer);
  int bit;
  object* subscripts;
  object* ox;
  object* oy;
  object* oyy;
  object** element;

  int bmpbit = 0;
  for (int ay = 0; ay < ah; ay++) {
    for (int ax = 0; ax < aw; ax++) {
      ox = number(ax);
      oy = number(ay);
      oyy = cons(oy, NULL);
      subscripts = cons(ox, oyy);
      element = getarray(array, subscripts, env, &bit);
      if (bit < 0) {
        error2("OLED draws monochrome image only");
      }
      else {
        bmpbit = abs(checkinteger(*element) & (1<<bit));
        if (bmpbit > 0) {
          oled.drawPixel(x+ax, y+ay);
        }
      }
      myfree(subscripts);
      myfree(oyy);
      myfree(oy);
      myfree(ox);
    }
  }
  oled.sendBuffer();

  return nil;
}
#endif


#if defined(rfm69)
/*
  Helper function:
  Prepare SDI for radio use. Not accessible via uLisp.
  (Not sure why this process is necessary - RFM69 driver probably changes SPI settings)
*/
void radioON () {
  SPI.begin();
  pinMode(PIN_RADIO_CS, OUTPUT);
  digitalWrite(PIN_RADIO_CS, LOW);
}

/*
  Helper function:
  Prepare SDI for other use. Not accessible via uLisp.
  (Not sure why this process is necessary - RFM69 driver probably changes SPI settings)
*/
void radioOFF () {
  pinMode(PIN_RADIO_CS, OUTPUT);
  digitalWrite(PIN_RADIO_CS, HIGH);
  SPI.begin();
  static SPISettings mySPISettings = SPISettings(1000000, MSBFIRST, SPI_MODE0);
}

/*
  (rfm69-begin)
  Start RFM69 radio module with pin and IRQ number assignments and reset it.
*/
object *fn_RFM69Begin (object *args, object *env) {
  (void) env;
 
  #if defined radiohead
    (void) args, (void) env;  //Radiohead library by default without sender/receiver and without network ID
  #else  
    int nodeid = checkinteger(first(args));
    int netid = checkinteger(second(args));
  #endif
  #if defined(rfm69) && defined(ARDUINO_PIMORONI_TINY2040)
    pinMode(RFM69_IRQ, INPUT_PULLUP); 
  #endif

  // Hard Reset the RFM module
  pinMode(RFM69_RST, OUTPUT);
  digitalWrite(RFM69_RST, LOW);

  // manual reset
  digitalWrite(RFM69_RST, HIGH);
  delay(100);
  digitalWrite(RFM69_RST, LOW);
  delay(100);

  pfun_t pf = pserial;

  #if defined radiohead
    bool result = radio.init();
    if (!result)
    {
        pfstring("RH_RFM69 - Module init failed!", pf);
        return nil;
    }
    else
    {
        radio.setFrequency(FREQUENCY);
    }
  #else
    while(!radio.initialize(FREQUENCY, nodeid, netid))
    {
        pfstring("try to init...", pf);
        delay(100);
    }
  #endif

  if (IS_RFM69HCW) // Only for RFM69HCW & HW!
  {
      #if defined radiohead
        radio.setTxPower(13, true);
      #else
        radio.setHighPower(); 
      #endif
  }

  #if !defined radiohead
    radio.setPowerLevel(31); // power output ranges from 0 (5dBm) to 31 (20dBm)
    radio.encrypt(ENCRYPTKEY);
  #else
    radio.setEncryptionKey((uint8_t*)ENCRYPTKEY);
  #endif
  
  
  radioOFF();
  
  if (IS_RFM69HCW)
  {
      pfstring("RFM69 HCW initialized!\n", pf);
  }
  else
  {
      pfstring("RFM69 initialized!\n", pf);
  }
  pint(FREQUENCY, pf);
  #if !defined radiohead
    pserial(' ');
    pint(nodeid, pf);
    pserial(' ');
    pint(netid, pf);
  #endif
  pserial('\n');

  return nil;
}


/*
  (rfm69-send)
  Send string data package to specified receiver ID optionally requesting hardware acknowledge.
*/
object *fn_RFM69Send (object *args, object *env) {
  (void) env;
  
  if (args != NULL) {

    int receiver;
    bool ack = false;

    #if !defined radiohead
      receiver = checkinteger(first(args));
      args = cdr(args);
    #endif
    cstring(first(args), packet, PACKETLENGTH+1);   //build c-string from uLisp string, therefore includes additional '\0'

    #if !defined radiohead
      args = cdr(args);
      if (args != NULL) {
        ack = (first(args) == nil) ? false : true;
        }
    #endif
      
    radioON();
    #if defined radiohead
      bool result = radio.send((uint8_t*)packet, strlen(packet)+1);   //use *real* packet length for send, i.e. string may be shorter than PACKETLENGTH. '\0' sent too!
      if (!result)
      {
        radioOFF();
        pstring("RH_RFM69 send failed!", (pfun_t)pserial);
        return nil;   
      }
    #else
      radio.send(receiver, packet, strlen(packet)+1, ack);  //use *real* packet length for send, i.e. string may be shorter than PACKETLENGTH. '\0' sent too!
    #endif
    radioOFF();
    pstring(packet, (pfun_t)pserial);
    return tee;    
  }
  radioOFF();
  return nil;
}

/*
  (rfm69-receive)
  Retrieve string data package if something has been received.
*/
object *fn_RFM69Receive (object *args, object *env) {
  (void) env; (void) args;

  radioON();

  #if defined radiohead
    if (radio.available())
    {      
          bool result = radio.recv((uint8_t*)packet, &pctlen);   // RadioHead lib stores length of received packet in predefined global variable
          radioOFF();
          if (result)
          {
            packet[pctlen] = 0;  // add null terminating string for conversion into uLisp string
            return lispstring(packet);
          }
          else
          {
            pstring("RH_RFM69 receive failed!", (pfun_t)pserial);
            return nil;
          }
          
    }
  #else
    if (radio.receiveDone())
    {
          radioOFF();
          return lispstring((char*)radio.DATA);
    }
  #endif
  radioOFF();
  return nil;
}

/*
  (rfm69-get-rssi)
  Obtain signal strength reported at last transmit.
*/
object *fn_RFM69GetRSSI (object *args, object *env) {
  (void) env; (void) args;

  #if defined radiohead
    object* rssi = number(radio.rssiRead());
  #else
    object* rssi = number(radio.RSSI);
  #endif

  radioOFF();
  return rssi;
}
#endif


#if defined(servolib)
/*
  (servo-attach snum pin [pwmin pwmax])
  Attach servo snum to pin. Optionally define new pulse width min/max in microseconds.
*/
object *fn_ServoAttach (object *args, object *env) {
  (void) env;
 
  int snum = checkinteger(first(args));   // zero based index!;
  int pin = checkinteger(second(args));
  int usmin = 544;
  int usmax = 2400;

  args = cdr(args);
  args = cdr(args);
  if (args != NULL) {
    usmin = checkinteger(first(args));
    args = cdr(args);
    if (args != NULL) {
      usmax = checkinteger(first(args));
    }
  }

  if(servolist == NULL) {
    if((servolist = (struct ulservo*)malloc(sizeof(struct ulservo))) == NULL) {
       pfstring("Out of memory", (pfun_t)pserial);
       return nil;
    }

    servolist->snum = snum;
    servolist->pin = pin;
    servolist->nextservo = NULL;
    servolist->servo = Servo();
    servolist->servo.attach(pin, usmin, usmax);
  }
  else {
    curservo = servolist;
    while(curservo->nextservo != NULL) {
      if ((curservo->snum == snum) || (curservo->pin == pin)) {
        pfstring("Servo number or pin already in use!", (pfun_t)pserial);
        return nil;
      }
      curservo = curservo->nextservo;
    }

    if ((curservo->snum == snum) || (curservo->pin == pin)) {
      pfstring("Servo number or pin already in use!", (pfun_t)pserial);
      return nil;
    }

    if((curservo->nextservo = (struct ulservo*)malloc(sizeof(struct ulservo))) == NULL) {
        pfstring("Out of memory", (pfun_t)pserial);
        return nil;
    }

    curservo = curservo->nextservo;

    curservo->snum = snum;
    curservo->pin = pin;
    curservo->servo = Servo();
    curservo->servo.attach(pin, usmin, usmax);
    curservo->nextservo = NULL;
  }
  return tee;
}

/*
  (servo-write)
  Set angle of servo snum in degrees (0 to 180).
*/
object *fn_ServoWrite (object *args, object *env) {
  (void) env;
 
  int snum = checkinteger(first(args));
  int angle = checkinteger(second(args));
  curservo = servolist;

  if (curservo != NULL) {

    while(curservo->snum != snum) {
      curservo = curservo->nextservo;
      if (curservo == NULL) break;
    }

    if(curservo != NULL) {
      curservo->servo.write(angle);
      return number(angle);
    }
  }

  pfstring("Servo not found", (pfun_t)pserial);
  return nil;
}

/*
  (servo-write-microseconds)
  Set angle of servo snum using a pulse width value in microseconds.
*/
object *fn_ServoWriteMicroseconds (object *args, object *env) {
  (void) env;
 
  int snum = checkinteger(first(args));
  int us = checkinteger(second(args));
  curservo = servolist;

  if (curservo != NULL) {

    while(curservo->snum != snum) {
      curservo = curservo->nextservo;
      if (curservo == NULL) break;
    }

    if(curservo != NULL) {
      curservo->servo.writeMicroseconds(us);
      return number(us);
    }
  }

  pfstring("Servo not found", (pfun_t)pserial);
  return nil;
}

/*
  (servo-read)
  Read current angle of servo snum in degrees.
*/
object *fn_ServoRead (object *args, object *env) {
  (void) env;

  int snum = checkinteger(first(args));
  curservo = servolist;

  if (curservo != NULL) {

    while(curservo->snum != snum) {
      curservo = curservo->nextservo;
      if (curservo == NULL) break;
    }

    if(curservo != NULL) {
      return number(curservo->servo.read());
    }
  }

  pfstring("Servo not found", (pfun_t)pserial);
  return nil;
}

/*
  (servo-detach)
  Detach servo snum, thus freeing the assigned pin for other tasks.
*/
object *fn_ServoDetach (object *args, object *env) {
  (void) env;

  int snum = checkinteger(first(args));
  curservo = servolist;
  struct ulservo* lastservo = servolist;

  if (curservo != NULL) {

    while(curservo->snum != snum) {
      lastservo = curservo;
      curservo = curservo->nextservo;
      if (curservo == NULL) break;
    }

    if(curservo != NULL) {
      curservo->servo.detach();
      if (curservo == servolist) {    // delete first element of list
        servolist = curservo->nextservo;
      }
      else {
        lastservo->nextservo = curservo->nextservo;
      }
      free(curservo);
      return tee;
    }
  }

  pfstring("Servo not found", (pfun_t)pserial);
  return nil;
}
#endif


#if defined(matrixlib)
/*
  (matrix-begin)
  Start I2C with address addr and initialize display state (blink off, full brightness).
*/
object *fn_MatrixBegin(object *args, object *env) {
  (void) env;
  int addr = 0x70;
  if (args != NULL) {
    addr = checkinteger(first(args));
  }

  TwoWire *theWire = &Wire;
  matrix.begin(addr, theWire);
  return nil;
}

/*
  (matrix-state [st])
  Turn display on or off
*/
object *fn_MatrixState(object *args, object *env) {
  (void) env;
  bool state = true;

  if (args != NULL) {
    state = (first(args) == nil) ? false : true;
  }
  matrix.setDisplayState(state);
  return nil;
}

/*
  (matrix-brightness [br])
  Set display to brightness br. 0 (min) to 15 (max).
*/
object *fn_MatrixBrightness(object *args, object *env) {
  (void) env;
  int br = 15;
  if (args != NULL) {
    br = checkinteger(first(args));
  }
  matrix.setBrightness(br);
  return nil;
}

/*
  (matrix-blink-rate [br])
  Set display blink rate hz. 0 = no blinking, 1 = 1 Hz, 2 = 2 Hz, 3 = 0.5 Hz.
*/
object *fn_MatrixBlinkRate(object *args, object *env) {
  (void) env;
  int br = 0;
  if (args != NULL) {
    br = checkinteger(first(args));
  }
  switch (br) {
    case 0:
      matrix.blinkRate(HT16K33_BLINK_OFF);
      break;
    case 1:
      matrix.blinkRate(HT16K33_BLINK_1HZ);
      break;
    case 2:
      matrix.blinkRate(HT16K33_BLINK_2HZ);
      break;
    case 3:
      matrix.blinkRate(HT16K33_BLINK_HALFHZ);
  }
  return nil;
}

/*
  (matrix-show)
  Issue buffered data in RAM to display.
*/
object *fn_MatrixShow(object *args, object *env) {
  (void) args, (void) env;
  matrix.writeDisplay();
  return nil;
}

/*
  (matrix-clear)
  Clear display.
*/
object *fn_MatrixClear(object *args, object *env) {
  (void) args, (void) env;
  matrix.clear();
  return nil;
}

/*
  (matrix-draw-rect x y w h col)
  Wrapper copied from uLisp GFX. See doc there.
*/
object *fn_MatrixDrawRect (object *args, object *env) {
  (void) env;
  uint16_t params[4], colour = LED_ON;
  for (int i=0; i<4; i++) { params[i] = checkinteger(car(args)); args = cdr(args); }
  if (args != NULL) colour = checkinteger(car(args));
  matrix.drawRect(params[0], params[1], params[2], params[3], colour);
  return nil;
}

/*
  (matrix-set-rotation r)
  Set rotation of screen, see Adafruit GFX.
*/
object *fn_MatrixSetRotation (object *args, object *env) {
  (void) env;
  matrix.setRotation(checkinteger(first(args)));
  return nil;
}

#endif

// Symbol names

#if defined (gfxsupport)
//Added to standard GFX support
const char stringSetBacklight[] PROGMEM = "set-backlight";
const char stringDisplayBMP[] PROGMEM = "display-bmp";
const char stringLoadBMP[] PROGMEM = "load-bmp";
const char stringLoadMono[] PROGMEM = "load-mono";
const char stringShowBMP[] PROGMEM = "show-bmp";
#endif

//USB host keyboard supported anytime
const char stringKeyboardGetKey[] PROGMEM = "keyboard-get-key";
const char stringKeyboardFlush[] PROGMEM = "keyboard-flush";
const char stringMouseGetValues[] PROGMEM = "mouse-get-values";
const char stringMouseLastButtons[] PROGMEM = "mouse-last-buttons";

//Vector math supported anytime
const char stringRadToDeg[] PROGMEM = "rad-to-deg";
const char stringDegToRad[] PROGMEM = "deg-to-rad";
const char stringVectorSub[] PROGMEM = "vector-sub";
const char stringVectorAdd[] PROGMEM = "vector-add";
const char stringVectorNorm[] PROGMEM = "vector-norm";
const char stringScalarMult[] PROGMEM = "scalar-mult";
const char stringDotProduct[] PROGMEM = "dot-product";
const char stringCrossProduct[] PROGMEM = "cross-product";
const char stringVectorAngle[] PROGMEM = "vector-angle";


//String helper function from M5Cardputer editor version by hasn0life
const char stringSearchStr[] PROGMEM = "search-str";

#if defined sdcardsupport
const char stringSDFileExists[] PROGMEM = "sd-file-exists";
const char stringSDFileRemove[] PROGMEM = "sd-file-remove";
const char stringSDCardDir[] PROGMEM = "sd-card-dir";
#endif

#if defined NEOPIXEL_NUM
const char stringPixelsBegin[] PROGMEM = "pixels-begin";
const char stringPixelsClear[] PROGMEM = "pixels-clear";
const char stringPixelsFill[] PROGMEM = "pixels-fill";
const char stringPixelsSetPixelColor[] PROGMEM = "pixels-set-pixel-color";
const char stringPixelsColor[] PROGMEM = "pixels-color";
const char stringPixelsColorHSV[] PROGMEM = "pixels-color-hsv";
const char stringPixelsShow[] PROGMEM = "pixels-show";
const char stringPixelsRainbow[] PROGMEM = "pixels-rainbow";
#endif

#if defined(RA8875_gfx)
const char stringTFT1Begin[] PROGMEM = "tft1-begin";
const char stringTFT1On[] PROGMEM = "tft1-on";
const char stringTFT1SetRotation[] PROGMEM = "tft1-set-rotation";
const char stringTFT1TextMode[] PROGMEM = "tft1-text-mode";
const char stringTFT1GraphicsMode[] PROGMEM = "tft1-graphics-mode";
const char stringTFT1SetCursor[] PROGMEM = "tft1-set-cursor";
const char stringTFT1SetTextColor[] PROGMEM = "tft1-set-text-color";
const char stringTFT1TextEnlarge[] PROGMEM = "tft1-text-enlarge";
const char stringTFT1WriteText[] PROGMEM = "tft1-write-text";
const char stringTFT1SetScrollWin[] PROGMEM = "tft1-set-scroll-win";
const char stringTFT1SetScrollY[] PROGMEM = "tft1-set-scrolly";
const char stringTFT1SetScrollX[] PROGMEM = "tft1-set-scrollx";
const char stringTFT1BlinkCursor[] PROGMEM = "tft1-blink-cursor";
const char stringTFT1DrawPixel[] PROGMEM = "tft1-draw-pixel";
const char stringTFT1DrawLine[] PROGMEM = "tft1-draw-line";
const char stringTFT1DrawRect[] PROGMEM = "tft1-draw-rect";
const char stringTFT1FillRect[] PROGMEM = "tft1-fill-rect";
const char stringTFT1DrawCircle[] PROGMEM = "tft1-draw-circle";
const char stringTFT1FillCircle[] PROGMEM = "tft1-fill-circle";
const char stringTFT1DrawEllipse[] PROGMEM = "tft1-draw-ellipse";
const char stringTFT1FillEllipse[] PROGMEM = "tft1-fill-ellipse";
const char stringTFT1DrawRoundRect[] PROGMEM = "tft1-draw-round-rect";
const char stringTFT1FillRoundRect[] PROGMEM = "tft1-fill-round-rect";
const char stringTFT1DrawTriangle[] PROGMEM = "tft1-draw-triangle";
const char stringTFT1FillTriangle[] PROGMEM = "tft1-fill-triangle";
const char stringTFT1DrawCurve[] PROGMEM = "tft1-draw-curve";
const char stringTFT1FillCurve[] PROGMEM = "tft1-fill-curve";
const char stringTFT1FillScreen[] PROGMEM = "tft1-fill-screen";
const char stringTFT1DisplayBMP[] PROGMEM = "tft1-display-bmp";
const char stringTFT1ShowBMP[] PROGMEM = "tft1-show-bmp";

const char stringTFT1ReadReg[] PROGMEM = "tft1-read-reg";
const char stringTFT1WriteReg[] PROGMEM = "tft1-write-reg";
const char stringTFT1ReadData[] PROGMEM = "tft1-read-data";
const char stringTFT1WriteData[] PROGMEM = "tft1-write-data";
const char stringTFT1ReadStatus[] PROGMEM = "tft1-read-status";
const char stringTFT1WriteCommand[] PROGMEM = "tft1-write-command";
const char stringTFT1SetBacklight[] PROGMEM = "tft1-set-backlight";

const char stringTouchBegin[] PROGMEM = "touch-begin";
const char stringTouchGetPoint[] PROGMEM = "touch-get-point";
const char stringTouchWaitForTouch[] PROGMEM = "touch-wait-for-touch";
#endif

#if defined(oled_gfx)
const char stringOledBegin[] PROGMEM = "oled-begin";
const char stringOledClear[] PROGMEM = "oled-clear";
const char stringOledSetRotation[] PROGMEM = "oled-set-rotation";
const char stringOledSetColor[] PROGMEM = "oled-set-color";
const char stringOledWriteChar[] PROGMEM = "oled-write-char";
const char stringOledWriteString[] PROGMEM = "oled-write-string";
const char stringOledDrawPixel[] PROGMEM = "oled-draw-pixel";
const char stringOledDrawLine[] PROGMEM = "oled-draw-line";
const char stringOledDrawHLine[] PROGMEM = "oled-draw-hline";
const char stringOledDrawVLine[] PROGMEM = "oled-draw-vline";
const char stringOledDrawRect[] PROGMEM = "oled-draw-rect";
const char stringOledFillRect[] PROGMEM = "oled-fill-rect";
const char stringOledDrawCircle[] PROGMEM = "oled-draw-circle";
const char stringOledFillCircle[] PROGMEM = "oled-fill-circle";
const char stringOledDrawRoundRect[] PROGMEM = "oled-draw-round-rect";
const char stringOledFillRoundRect[] PROGMEM = "oled-fill-round-rect";
const char stringOledFillTriangle[] PROGMEM = "oled-fill-triangle";
const char stringOledDisplayBMP[] PROGMEM = "oled-display-bmp";
const char stringOledShowBMP[] PROGMEM = "oled-show-bmp";
#endif

#if defined(rfm69)
const char stringRFM69Begin[] PROGMEM = "rfm69-begin";
const char stringRFM69Send[] PROGMEM = "rfm69-send";
const char stringRFM69Receive[] PROGMEM = "rfm69-receive";
const char stringRFM69GetRSSI[] PROGMEM = "rfm69-get-rssi";
#endif

#if defined(servolib)
const char stringServoAttach[] PROGMEM = "servo-attach";
const char stringServoWrite[] PROGMEM = "servo-write";
const char stringServoWriteMicroseconds[] PROGMEM = "servo-write-microseconds";
const char stringServoRead[] PROGMEM = "servo-read";
const char stringServoDetach[] PROGMEM = "servo-detach";
#endif

#if defined(matrixlib)
const char stringMatrixBegin[] PROGMEM = "matrix-begin";
const char stringMatrixClear[] PROGMEM = "matrix-clear";
const char stringMatrixState[] PROGMEM = "matrix-state";
const char stringMatrixBrightness[] PROGMEM = "matrix-brightness";
const char stringMatrixBlinkRate[] PROGMEM = "matrix-blink-rate";
const char stringMatrixShow[] PROGMEM = "matrix-show";
const char stringMatrixDrawRect[] PROGMEM = "matrix-draw-rect";
const char stringMatrixSetRotation[] PROGMEM = "matrix-set-rotation";
#endif


// Documentation strings
#if defined(gfxsupport)
//added to standard GFX support
const char docSetBacklight[] PROGMEM = "(set-backlight level)\n"
"Set backlight level of standard GFX display.";
const char docDisplayBMP[] PROGMEM = "(display-bmp fname x y)\n"
"Open BMP file fname from SD if it exits and display it on screen at position x y.";
const char docLoadBMP[] PROGMEM = "(load-bmp fname arr [offx] [offy])\n"
"Open BMP file fname from SD if it exits and copy it into the two-dimensional uLisp array provided.\n"
"Note that this allocates massive amounts of RAM. Use for small bitmaps/icons only.\n"
"When the image is larger than the array, only the upper leftmost area of the bitmap fitting into the array is loaded.\n"
"Providing offx and offy you may move the 'window' of the array to other parts of the bitmap (useful e.g. for tiling).";
const char docLoadMono[] PROGMEM = "(load-mono fname arr [offx] [offy])\n"
"Open monochrome BMP file fname from SD if it exits and copy it into the two-dimensional uLisp bit array provided.\n"
"Note that this allocates massive amounts of RAM. Use for small bitmaps/icons only.\n"
"When the image is larger than the array, only the upper leftmost area of the bitmap fitting into the array is loaded.\n"
"Providing offx and offy you may move the 'window' of the array to other parts of the bitmap (useful e.g. for tiling).";
const char docShowBMP[] PROGMEM = "(show-bmp arr x y [monocol])\n"
"Show bitmap image contained in uLisp array arr on screen at position x y.\n"
"The function automatically distinguishes between monochrome and color image arrays.\n"
"If monocol is provided, a monochrome image is painted with that color value.";
#endif

//USB host keyboard and mouse supported anytime
const char docKeyboardGetKey[] PROGMEM = "(keyboard-get-key [pressed])\n"
"Get key last recognized - default: when released, if [pressed] is t: when pressed).";
const char docKeyboardFlush[] PROGMEM = "(keyboard-flush)\n"
"Discard missing key up/down events.";
const char docMouseGetValues[] PROGMEM = "(mouse-get-values)\n"
"Query HID mouse interface and return values relx, rely, buttons, relwheel, relwheelH as a list.";
const char docMouseLastButtons[] PROGMEM = "(mouse-last-buttons)\n"
"Returns last mouse button status without retrieving current values.";

//Vector math supported anytime
const char docRadToDeg[] PROGMEM = "(rad-to-deg n)\n"
"Convert radians to degrees.";
const char docDegToRad[] PROGMEM = "(deg-to-rad n)\n"
"Convert degree to radians.";
const char docVectorSub[] PROGMEM = "(vector-sub v1 v2)\n"
"Subtract vector v2 from vector v1 (lists with 3 elements).";
const char docVectorAdd[] PROGMEM = "(vector-add v1 v2)\n"
"Add vector v2 to vector v1 (lists with 3 elements).";
const char docVectorNorm[] PROGMEM = "(vector-norm v)\n"
"Calculate magnitude/norm of vector v (list with 3 elements).";
const char docScalarMult[] PROGMEM = "(scalar-mult v s)\n"
"Multiply vector v (list with 3 elements) by number s (scalar).";
const char docDotProduct[] PROGMEM = "(dot-product v1 v2)\n"
"Calculate dot product of two three-dimensional vectors v1, v2 (lists with 3 elements).";
const char docCrossProduct[] PROGMEM = "(cross-product v1 v2)\n"
"Calculate cross product of two three-dimensional vectors v1, v2 (lists with 3 elements).";
const char docVectorAngle[] PROGMEM = "(vector-angle v1 v2)\n"
"Calculate angle (rad) between two three-dimensional vectors v1, v2 (lists with 3 elements).";

//String helper function from M5Cardputer editor version by hasn0life
const char docSearchStr[] PROGMEM = "(search-str pattern target [startpos])\n"
"Returns the index of the first occurrence of pattern in target, or nil if it's not found\n"
"starting from startpos";

#if defined sdcardsupport
const char docSDFileExists[] PROGMEM = "(sd-file-exists filename)\n"
"Returns t if filename exists on SD card, otherwise nil.";
const char docSDFileRemove[] PROGMEM = "(sd-file-remove filename)\n"
"Delete file with filename. Returns t if successful, otherwise nil.";
const char docSDCardDir[] PROGMEM = "(sd-card-dir)\n"
"Print SD card directory and return nothing [mode 0, optional] or a uLisp string object [mode 1] or a List [mode 2].";
#endif
#if defined NEOPIXEL_NUM
const char docPixelsBegin[] PROGMEM = "(pixels-begin)\n"
"Configures the NeoPixel pin for output.";
const char docPixelsClear[] PROGMEM = "(pixels-clear)\n"
"Sets all pixel colors to 'off'.";
const char docPixelsFill[] PROGMEM = "(pixels-fill [rgbw] [first] [fill])\n"
"Fills all or part of the NeoPixel strip with a fixed 32-bit packed RGB or RGBW value (default 0).\n"
"first, default 0, the first NeoPixel to fill.\n"
"fill, default all, the number of NeoPixels to fill.";
const char docPixelsSetPixelColor[] PROGMEM = "(pixels-set-pixel-color index rgbw)\n"
"(pixels-set-pixel-color index red green blue [white])\n"
"Sets a pixel's color using either a 32-bit packed RGB or RGBW value,\n"
"or separate red, green, blue, and optionally white components.";
const char docPixelsColor[] PROGMEM = "(pixels-color red green blue [white])\n"
"Converts separate red, green, blue, and optionally white values into\n"
"a single packed 32-bit RGB or RGBW color.";
const char docPixelsColorHSV[] PROGMEM = "(pixels-color-hsv hue sat val)\n"
"Converts separate hue (0 to 65535), saturation (0 to 255), and value (0 to 255) values into\n"
"a single packed 32-bit RGB or RGBW color.";
const char docPixelsShow[] PROGMEM = "(pixels-show)\n"
"Transmits the pixel data to the NeoPixels.";
const char docPixelsRainbow[] PROGMEM = "(pixels-rainbow [first-hue] [cycles] [saturation] [brightness] [gammify])\n"
"Fills the NeoPixel strip with one or more cycles of hues.\n"
"first-hue, default 0, is the hue of the first pixel (0 to 65535).\n"
"cycles, default 1, is the number of cycles.\n"
"saturation, default 255, is the saturation (0 to 255).\n"
"brightness, default 255, is the brightness (0 to 255).\n"
"gammify, default true, applies gamma correction to colours.";
#endif

#if defined(RA8875_gfx)
const char docTFT1Begin[] PROGMEM = "(tft1-begin)\n"
"Initialize tft screen controller.";
const char docTFT1On[] PROGMEM = "(tft1-on)\n"
"Switch TFT and backlight on and fill screen with black.";
const char docTFT1SetRotation[] PROGMEM = "(tft1-set-rotation rot_code)\n"
"Set rotation of screen, see Adafruit GFX.";
const char docTFT1TextMode[] PROGMEM = "(tft1-text-mode)\n"
"Switch to text mode.";
const char docTFT1GraphicsMode[] PROGMEM = "(tft1-graphics-mode)\n"
"Switch to graphics mode.";
const char docTFT1SetCursor[] PROGMEM = "(tft1-set-cursor x y)\n"
"Set text cursor to location x (column) y (row).";
const char docTFT1SetTextColor[] PROGMEM = "(tft1-set-text-color fg [bg])\n"
"Set foreground and background colors for text display. Colors are a 565-coded 16-bit-value.";
const char docTFT1TextEnlarge[] PROGMEM = "(tft1-text-enlarge scale)\n"
"Enlarge text (0 is standard size).";
const char docTFT1WriteText[] PROGMEM = "(tft1-write-text str)\n"
"Write string str to screen.";
const char docTFT1SetScrollWin[] PROGMEM = "(tft1-set-scroll-win x y width height mode)\n"
"Set scroll window and scroll mode.";
const char docTFT1SetScrollY[] PROGMEM = "(tft1-set-scrolly l)\n"
"Scroll screen vertically l pixel lines.";
const char docTFT1SetScrollX[] PROGMEM = "(tft1-set-scrollx c)\n"
"Scroll screen horizontally c pixel columns.";
const char docTFT1BlinkCursor[] PROGMEM = "(tft1-blink-cursor bool)\n"
"If bool then switch on blinking cursor, else switch it off.";
const char docTFT1DrawPixel[] PROGMEM = "(tft1-draw-pixel ...)\n"
"Wrapper copied from uLisp GFX. See doc there.";
const char docTFT1DrawLine[] PROGMEM = "(tft1-draw-line ...)\n"
"Wrapper copied from uLisp GFX. See doc there.";
const char docTFT1DrawRect[] PROGMEM = "(tft1-draw-rect ...)\n"
"Wrapper copied from uLisp GFX. See doc there.";
const char docTFT1FillRect[] PROGMEM = "(tft1-fill-rect ...)\n"
"Wrapper copied from uLisp GFX. See doc there.";
const char docTFT1DrawCircle[] PROGMEM = "(tft1-draw-circle ...)\n"
"Wrapper copied from uLisp GFX. See doc there.";
const char docTFT1FillCircle[] PROGMEM = "(tft1-fill-circle ...)\n"
"Wrapper copied from uLisp GFX. See doc there.";
const char docTFT1DrawEllipse[] PROGMEM = "(tft1-draw-ellipse x y long short [col])\n"
"Draw ellipse at position x y with long arm and short arm.";
const char docTFT1FillEllipse[] PROGMEM = "(tft1-fill-ellipse x y long short [col])\n"
"Fill ellipse at position x y with long arm and short arm.";
const char docTFT1DrawRoundRect[] PROGMEM = "(tft1-draw-round-rect ...)\n"
"Wrapper copied from uLisp GFX. See doc there.";
const char docTFT1FillRoundRect[] PROGMEM = "(tft1-fill-round-rect ...)\n"
"Wrapper copied from uLisp GFX. See doc there.";
const char docTFT1DrawTriangle[] PROGMEM = "(tft1-draw-triangle ...)\n"
"Wrapper copied from uLisp GFX. See doc there.";
const char docTFT1FillTriangle[] PROGMEM = "(tft1-fill-triangle ...)\n"
"Wrapper copied from uLisp GFX. See doc there.";
const char docTFT1DrawCurve[] PROGMEM = "(tft1-draw-curve x y long short curve [col])\n"
"Draw curve at position x y with long arm, short arm and curve part.";
const char docTFT1FillCurve[] PROGMEM = "(tft1-fill-curve x y long short curve [col])\n"
"Fill curve at position x y with long arm, short arm and curve part.";
const char docTFT1FillScreen[] PROGMEM = "(tft1-fill-screen ...)\n"
"Wrapper copied from uLisp GFX. See doc there.";
const char docTFT1DisplayBMP[] PROGMEM = "(tft1-display-bmp fname x y)\n"
"Opens file fname from SD if it exits and displays it on screen at position x y.";
const char docTFT1ShowBMP[] PROGMEM = "(tft1-show-bmp arr x y [monocol])\n"
"Show bitmap image contained in uLisp array arr on screen at position x y.\n"
"The function automatically distinguishes between monochrome and color image arrays.\n"
"If monocol is provided, a monochrome image is painted with that color value.";

const char docTFT1WriteReg[] PROGMEM = "(tft1-write-reg reg val)\n"
"Low-level access: Write val to register reg.";
const char docTFT1ReadReg[] PROGMEM = "(tft1-read-reg reg)\n"
"Low-level access: Read register reg.";
const char docTFT1WriteData[] PROGMEM = "(tft1-write-data d)\n"
"Low-level access: Write data d to current register.";
const char docTFT1ReadData[] PROGMEM = "(tft1-read-data)\n"
"Low-level access: Read data value from current register.";
const char docTFT1WriteCommand[] PROGMEM = "(tft1-write-command c)\n"
"Low-level access: Write command c to current register.";
const char docTFT1ReadStatus[] PROGMEM = "(tft1-read-status)\n"
"Low-level access: Read status from current register.";
const char docTFT1SetBacklight[] PROGMEM = "(tft1-set-backlight level)\n"
"Set backlight to level [0-255].";

const char docTouchBegin[] PROGMEM = "(touch-begin)\n"
"Initialize touch screen controller.";
const char docTouchGetPoint[] PROGMEM = "(touch-get-point)\n"
"Return touched x,y position as a list.";
const char docTouchWaitForTouch[] PROGMEM = "(touch-wait-for-touch)\n"
"Wait for touch screen response and return touched x,y position as a list.\n";
#endif

#if defined(oled_gfx)
const char docOledBegin[] PROGMEM = "(oled-begin adr)\n"
"Initialize OLED (optionally using I2C address adr, otherwise using default address #x3C (7 bit)/#x78 (8 bit)).";
const char docOledClear[] PROGMEM = "(oled-clear)\n"
"Clear OLED.";
const char docOledSetRotation[] PROGMEM = "(oled-set-rotation rot)\n"
"Set rotation of screen. 0 = no rotation, 1 = 90 degrees, 2 = 180 degrees, 3 = 270 degrees, 4 = hor. mirrored, 5 = vert. mirrored.";
const char docOledSetColor[] PROGMEM = "(oled-set-color fg)\n"
"Set foreground color for text and graphics. Colors are 0 (clear/black), 1 (set/white) and 2 (XOR).";
const char docOledWriteChar[] PROGMEM = "(oled-write-char x y c)\n"
"Write char c to screen at location x y.";
const char docOledWriteString[] PROGMEM = "(oled-write-string x y str)\n"
"Write string str to screen at location x y.";
const char docOledDrawPixel[] PROGMEM = "(oled-draw-pixel x y)\n"
"Draw pixel at position x y (using color set before)";
const char docOledDrawLine[] PROGMEM = "(oled-draw-line x0 y0 x1 y1)\n"
"Draw a line between positions x0/y0 and x1/y1.";
const char docOledDrawHLine[] PROGMEM = "(oled-draw-hline x y w)\n"
"Draw fast horizontal line at position X Y with length w.";
const char docOledDrawVLine[] PROGMEM = "(oled-draw-vline x y h)\n"
"Draw fast vertical line at position X Y with length h.";
const char docOledDrawRect[] PROGMEM = "(oled-draw-rect x y w h)\n"
"Draw empty rectangle at x y with width w and height h.";
const char docOledFillRect[] PROGMEM = "(oled-fill-rect x y w h)\n"
"Draw empty rectangle at x y with width w and height h.";
const char docOledDrawCircle[] PROGMEM = "(oled-draw-circle x y r)\n"
"Draw empty circle at position x y with radius r.";
const char docOledFillCircle[] PROGMEM = "(oled-fill-circle x y r)\n"
"Draw filled circle at position x y with radius r.";
const char docOledDrawRoundRect[] PROGMEM = "(oled-draw-round-rect x y w h r)\n"
"Draw empty rectangle at x y with width w and height h. Edges are rounded with radius r.";
const char docOledFillRoundRect[] PROGMEM = "(oled-fill-round-rect x y w h r)\n"
"Draw filled rectangle at x y with width w and height h. Edges are rounded with radius r.";
const char docOledFillTriangle[] PROGMEM = "(oled-fill-triangle x0 y0 x1 y1 x2 y2)\n"
"Draw filled triangle with corners at x0/y0, x1/y1 and x2/y2.";
const char docOledDisplayBMP[] PROGMEM = "(oled-display-bmp fname x y)\n"
"Open monochrome BMP file fname from SD if it exits and display it on screen at position x y (using the color set before).";
const char docOledShowBMP[] PROGMEM = "(oled-show-bmp arr x y)\n"
"Show bitmap image contained in monochrome uLisp array arr on screen at position x y (using color set before).";
#endif

#if defined(rfm69)
const char docRFM69Begin[] PROGMEM = "(rfm69-begin nodeid netid)\n"
"Reset RFM69 module and initialize communication with frequency band, node id and net id.";
const char docRFM69Send[] PROGMEM = "(rfm69-send receiver pckgstr [ack])\n"
"Send string data package to specified receiver ID optionally requesting hardware acknowledge.";
const char docRFM69Receive[] PROGMEM = "(rfm69-receive)\n"
"Retrieve string data package if something has been received.";
const char docRFM69GetRSSI[] PROGMEM = "(rfm69-get-rssi)\n"
"Obtain signal strength reported at last transmit.";
#endif

#if defined(servolib)
const char docServoAttach[] PROGMEM = "(servo-attach snum pin usmin usmax)\n"
"Attach servo snum to pin. Optionally define new pulse width min/max in microseconds.";
const char docServoWrite[] PROGMEM = "(servo-write snum angle)\n"
"Set angle of servo snum in degrees (0 to 180).";
const char docServoWriteMicroseconds[] PROGMEM = "(servo-write snum usecs)\n"
"Set angle of servo snum using a pulse width value in microseconds.";
const char docServoRead[] PROGMEM = "(servo-read snum)\n"
"Read current angle of servo snum in degrees.";
const char docServoDetach[] PROGMEM = "(servo-detach snum)\n"
"Detach servo snum, thus freeing the assigned pin for other tasks.";
#endif

#if defined(matrixlib)
const char docMatrixBegin[] PROGMEM = "(matrix-begin)\n"
"Start I2C with address addr and initialize display state (blink off, full brightness).";
const char docMatrixClear[] PROGMEM = "(matrix-clear)\n"
"Clear display.";
const char docMatrixState[] PROGMEM = "(matrix-state [st])\n"
"Turn display on or off";
const char docMatrixBrightness[] PROGMEM = "(matrix-brightness [br])\n"
"Set display to brightness br. 0 (min) to 15 (max).";
const char docMatrixBlinkRate[] PROGMEM = "(matrix-blink-rate [br])\n"
"Set display blink rate hz. 0 = no blinking, 1 = 1 Hz, 2 = 2 Hz, 3 = 0.5 Hz.";
const char docMatrixShow[] PROGMEM = "(matrix-show)\n"
"Issue buffered data in RAM to display.";
const char docMatrixDrawRect[] PROGMEM = "(matrix-draw-rect x y w h col)\n"
"Wrapper copied from uLisp GFX. See doc there.";
const char docMatrixSetRotation[] PROGMEM = "(matrix-set-rotation r)\n"
"Set rotation of screen, see Adafruit GFX.";
#endif

// Symbol lookup table
const tbl_entry_t lookup_table2[] PROGMEM = {

#if defined(gfxsupport)
{ stringSetBacklight, fn_SetBacklight, 0211, docSetBacklight },
{ stringDisplayBMP, fn_DisplayBMP, 0233, docDisplayBMP },
{ stringLoadBMP, fn_LoadBMP, 0224, docLoadBMP },
{ stringLoadMono, fn_LoadMono, 0224, docLoadMono },
{ stringShowBMP, fn_ShowBMP, 0234, docShowBMP },
#endif

{ stringKeyboardGetKey, fn_KeyboardGetKey, 0201, docKeyboardGetKey },
{ stringKeyboardFlush, fn_KeyboardFlush, 0200, docKeyboardFlush },
{ stringMouseGetValues, fn_MouseGetValues, 0200, docMouseGetValues },
{ stringMouseLastButtons, fn_MouseLastButtons, 0200, docMouseLastButtons },
{ stringSearchStr, fn_searchstr, 0224, docSearchStr },

{ stringRadToDeg, fn_RadToDeg, 0211, docRadToDeg },
{ stringDegToRad, fn_DegToRad, 0211, docDegToRad },
{ stringVectorSub, fn_VectorSub, 0222, docVectorSub },
{ stringVectorAdd, fn_VectorAdd, 0222, docVectorAdd },
{ stringVectorNorm, fn_VectorNorm, 0211, docVectorNorm },
{ stringScalarMult, fn_ScalarMult, 0222, docScalarMult },
{ stringDotProduct, fn_DotProduct, 0222, docDotProduct },
{ stringCrossProduct, fn_CrossProduct, 0222, docCrossProduct },
{ stringVectorAngle, fn_VectorAngle, 0222, docVectorAngle },

#if defined sdcardsupport
  { stringSDFileExists, fn_SDFileExists, 0211, docSDFileExists },
  { stringSDFileRemove, fn_SDFileRemove, 0211, docSDFileRemove },
  { stringSDCardDir, fn_SDCardDir, 0201, docSDCardDir },
#endif

#if defined NEOPIXEL_NUM
  { stringPixelsBegin, fn_PixelsBegin, 0200, docPixelsBegin },
  { stringPixelsClear, fn_PixelsClear, 0200, docPixelsClear },
  { stringPixelsFill, fn_PixelsFill, 0203, docPixelsFill },
  { stringPixelsSetPixelColor, fn_PixelsSetPixelColor, 0225, docPixelsSetPixelColor },
  { stringPixelsColor, fn_PixelsColor, 0234, docPixelsColor },
  { stringPixelsColorHSV, fn_PixelsColorHSV, 0233, docPixelsColorHSV },
  { stringPixelsShow, fn_PixelsShow, 0200, docPixelsShow },
  { stringPixelsRainbow, fn_PixelsRainbow, 0205, docPixelsRainbow },
#endif

#if defined(RA8875_gfx)
  { stringTFT1Begin, fn_TFT1Begin, 0200, docTFT1Begin },
  { stringTFT1On, fn_TFT1On, 0200, docTFT1On },
  { stringTFT1SetRotation, fn_TFT1SetRotation, 0211, docTFT1SetRotation },
  { stringTFT1TextMode, fn_TFT1TextMode, 0200, docTFT1TextMode },
  { stringTFT1GraphicsMode, fn_TFT1GraphicsMode, 0200, docTFT1GraphicsMode },
  { stringTFT1SetCursor, fn_TFT1SetCursor, 0222, docTFT1SetCursor },
  { stringTFT1SetTextColor, fn_TFT1SetTextColor, 0212, docTFT1SetTextColor },
  { stringTFT1TextEnlarge, fn_TFT1TextEnlarge, 0211, docTFT1TextEnlarge },
  { stringTFT1WriteText, fn_TFT1WriteText, 0211, docTFT1WriteText },
  { stringTFT1SetScrollWin, fn_TFT1SetScrollWin, 0245, docTFT1SetScrollWin},
  { stringTFT1SetScrollY, fn_TFT1SetScrollY, 0211, docTFT1SetScrollY },
  { stringTFT1SetScrollX, fn_TFT1SetScrollX, 0211, docTFT1SetScrollX },
  { stringTFT1BlinkCursor, fn_TFT1BlinkCursor, 0211, docTFT1BlinkCursor },
  { stringTFT1DrawPixel, fn_TFT1DrawPixel, 0223, docTFT1DrawPixel },
  { stringTFT1DrawLine, fn_TFT1DrawLine, 0245, docTFT1DrawLine },
  { stringTFT1DrawRect, fn_TFT1DrawRect, 0245, docTFT1DrawRect },
  { stringTFT1FillRect, fn_TFT1FillRect, 0245, docTFT1FillRect },
  { stringTFT1DrawCircle, fn_TFT1DrawCircle, 0234, docTFT1DrawCircle },
  { stringTFT1FillCircle, fn_TFT1FillCircle, 0234, docTFT1FillCircle },
  { stringTFT1DrawEllipse, fn_TFT1DrawEllipse, 0245, docTFT1DrawEllipse },
  { stringTFT1FillEllipse, fn_TFT1FillEllipse, 0245, docTFT1FillEllipse },
  { stringTFT1DrawRoundRect, fn_TFT1DrawRoundRect, 0256, docTFT1DrawRoundRect },
  { stringTFT1FillRoundRect, fn_TFT1FillRoundRect, 0256, docTFT1FillRoundRect },
  { stringTFT1DrawTriangle, fn_TFT1DrawTriangle, 0267, docTFT1DrawTriangle },
  { stringTFT1FillTriangle, fn_TFT1FillTriangle, 0267, docTFT1FillTriangle },
  { stringTFT1DrawCurve, fn_TFT1DrawCurve, 0256, docTFT1DrawCurve },
  { stringTFT1FillCurve, fn_TFT1FillCurve, 0256, docTFT1FillCurve },
  { stringTFT1FillScreen, fn_TFT1FillScreen, 0201, docTFT1FillScreen },
  { stringTFT1DisplayBMP, fn_TFT1DisplayBMP, 0233, docTFT1DisplayBMP },
  { stringTFT1ShowBMP, fn_TFT1ShowBMP, 0234, docTFT1ShowBMP },

  { stringTFT1WriteReg, fn_TFT1WriteReg, 0222, docTFT1WriteReg },
  { stringTFT1ReadReg, fn_TFT1ReadReg, 0211, docTFT1ReadReg },
  { stringTFT1WriteData, fn_TFT1WriteData, 0211, docTFT1WriteData },
  { stringTFT1ReadData, fn_TFT1ReadData, 0200, docTFT1ReadData },
  { stringTFT1WriteCommand, fn_TFT1WriteCommand, 0211, docTFT1WriteCommand },
  { stringTFT1ReadStatus, fn_TFT1ReadStatus, 0200, docTFT1ReadStatus },
  { stringTFT1SetBacklight, fn_TFT1SetBacklight, 0211, docTFT1SetBacklight },

  { stringTouchBegin, fn_TouchBegin, 0200, docTouchBegin },
  { stringTouchGetPoint, fn_TouchGetPoint, 0200, docTouchGetPoint },
  { stringTouchWaitForTouch, fn_TouchWaitForTouch, 0200, docTouchWaitForTouch },
#endif

#if defined(oled_gfx)
  { stringOledBegin, fn_OledBegin, 0201, docOledBegin },
  { stringOledClear, fn_OledClear, 0200, docOledClear },
  { stringOledSetRotation, fn_OledSetRotation, 0211, docOledSetRotation },
  { stringOledSetColor, fn_OledSetColor, 0211, docOledSetColor },
  { stringOledWriteChar, fn_OledWriteChar, 0233, docOledWriteChar },
  { stringOledWriteString, fn_OledWriteString, 0233, docOledWriteString },
  { stringOledDrawPixel, fn_OledDrawPixel, 0222, docOledDrawPixel },
  { stringOledDrawLine, fn_OledDrawLine, 0244, docOledDrawLine },
  { stringOledDrawHLine, fn_OledDrawHLine, 0233, docOledDrawHLine },
  { stringOledDrawVLine, fn_OledDrawVLine, 0233, docOledDrawVLine },
  { stringOledDrawRect, fn_OledDrawRect, 0244, docOledDrawRect },
  { stringOledFillRect, fn_OledFillRect, 0244, docOledFillRect },
  { stringOledDrawCircle, fn_OledDrawCircle, 0233, docOledDrawCircle },
  { stringOledFillCircle, fn_OledFillCircle, 0233, docOledFillCircle },
  { stringOledDrawRoundRect, fn_OledDrawRoundRect, 0255, docOledDrawRoundRect },
  { stringOledFillRoundRect, fn_OledFillRoundRect, 0255, docOledFillRoundRect },
  { stringOledFillTriangle, fn_OledFillTriangle, 0266, docOledFillTriangle },
  { stringOledDisplayBMP, fn_OledDisplayBMP, 0233, docOledDisplayBMP },
  { stringOledShowBMP, fn_OledShowBMP, 0233, docOledShowBMP },
#endif

#if defined(rfm69)
  #if defined radiohead
    { stringRFM69Begin, fn_RFM69Begin, 0200, docRFM69Begin },
    { stringRFM69Send, fn_RFM69Send, 0211, docRFM69Send },
  #else
    { stringRFM69Begin, fn_RFM69Begin, 0222, docRFM69Begin },
    { stringRFM69Send, fn_RFM69Send, 0223, docRFM69Send },
  #endif
  { stringRFM69Receive, fn_RFM69Receive, 0200, docRFM69Receive },
  { stringRFM69GetRSSI, fn_RFM69GetRSSI, 0200, docRFM69GetRSSI },
#endif

#if defined(servolib)
  { stringServoAttach, fn_ServoAttach, 0224, docServoAttach },
  { stringServoWrite, fn_ServoWrite, 0222, docServoWrite },
  { stringServoWriteMicroseconds, fn_ServoWriteMicroseconds, 0222, docServoWriteMicroseconds },
  { stringServoRead, fn_ServoRead, 0211, docServoRead },
  { stringServoDetach, fn_ServoDetach, 0211, docServoDetach },
#endif

#if defined(matrixlib)
  { stringMatrixBegin, fn_MatrixBegin, 0201, docMatrixBegin },
  { stringMatrixClear, fn_MatrixClear, 0200, docMatrixClear },
  { stringMatrixState, fn_MatrixState, 0201, docMatrixState },
  { stringMatrixBrightness, fn_MatrixBrightness, 0201, docMatrixBrightness },
  { stringMatrixBlinkRate, fn_MatrixBlinkRate, 0201, docMatrixBlinkRate },
  { stringMatrixShow, fn_MatrixShow, 0200, docMatrixShow },
  { stringMatrixDrawRect, fn_MatrixDrawRect, 0245, docMatrixDrawRect },
  { stringMatrixSetRotation, fn_MatrixSetRotation, 0211, docMatrixSetRotation },
#endif
};

// Table cross-reference functions - do not edit below this line

tbl_entry_t *tables[] = {lookup_table, lookup_table2};
const unsigned int tablesizes[] = { arraysize(lookup_table), arraysize(lookup_table2) };

const tbl_entry_t *table (int n) {
  return tables[n];
}

unsigned int tablesize (int n) {
  return tablesizes[n];
}
