unit UnitScreenEmulator;
{$ifdef fpc}
 {$mode delphi}
 {$ifdef cpu386}
  {$asmmode intel}
 {$endif}
 {$ifdef cpuamd64}
  {$asmmode intel}
 {$endif}
{$else}
 {$ifdef conditionalexpressions}
  {$if CompilerVersion>=24.0}
   {$legacyifend on}
  {$ifend}
 {$endif}
{$endif}
{$if defined(Win32) or defined(Win64)}
 {$define Windows}
{$ifend}

{$if defined(fpc)}
 {$optimization level1}
{$ifend}

interface

uses SysUtils,
     Classes,
     Math,
     Vulkan,
     PUCU,
     PasMP,
     {$ifdef unix}
     pthreads,
     {$endif}
     PasVulkan.Types,
     PasVulkan.Math,
     PasVulkan.Framework,
     PasVulkan.Application,
     PasVulkan.Sprites,
     PasVulkan.Canvas,
     PasVulkan.Font,
     PasVulkan.TrueTypeFont,
     PasVulkan.HighResolutionTimer,
     PasRISCV,
     PasTerm,
     VGAFont;

type { TMachineInstance }
     TMachineInstance=class(TPasMPThread)
      public
       const KERNEL_OFFSET=$200000;
             FontWidth=8;
             FontHeight=16;
             ScreenWidth=640;
             ScreenHeight=400;
             CanvasWidth=ScreenWidth*4;
             CanvasHeight=ScreenHeight*4;
       type TFrameBuffer=array[0..(ScreenWidth*ScreenHeight)-1] of TpvUInt32;
            TFrameBufferItem=record
             Width:TpvInt32;
             Height:TpvInt32;
             Active:Boolean;
             Data:TFrameBuffer;
            end;
            PFrameBufferItem=^TFrameBufferItem;
            TFrameBufferItems=array[0..3] of TFrameBufferItem;
            TIntegers=array of TpvInt32;
      private
       fMachineConfiguration:TPasRISCV.TConfiguration;
       fMachine:TPasRISCV;
       fFileSystem:TPasRISCV9PFileSystem;
       fEthernetDevice:TPasRISCVEthernetDevice;
       fNextFrameTime:TpvHighResolutionTime;
       fFrameBufferItems:TFrameBufferItems;
       fFrameBufferReadIndex:TpvInt32;
       fFrameBufferWriteIndex:TpvInt32;
       fXCacheIntegers:TIntegers;
       procedure ResetFrameBuffer;
      protected
       procedure Execute; override;
      public
       constructor Create;
       destructor Destroy; override;
       procedure Shutdown;
       procedure Boot;
       procedure OnReboot;
       procedure OnNewFrame;
       function TransferFrame(const aFrameBuffer:Pointer;out aActive:Boolean):boolean;
     end;

     { TScreenEmulator }

     TScreenEmulator=class(TpvApplicationScreen)
      public
      private
       fGraphicsFrameBuffer:TMachineInstance.TFrameBuffer;
       fTerminalFrameBuffer:TMachineInstance.TFrameBuffer;
       fFrameBuffers:array[0..MaxInFlightFrames-1] of TMachineInstance.TFrameBuffer;
       fFrameBufferTextures:array[0..MaxInFlightFrames-1] of TpvVulkanTexture;
       fFrameBufferGeneration:TpvUInt64;
       fFrameBufferGenerations:array[0..MaxInFlightFrames-1] of TpvUInt64;
       fFrameBufferTextureGenerations:array[0..MaxInFlightFrames-1] of TpvUInt64;
       fFrameBufferTextureSampler:TpvVulkanSampler;
       procedure DrawBackground(const aSender:TPasTerm);
       procedure DrawCodePoint(const aSender:TPasTerm;const aCodePoint:TPasTerm.TFrameBufferCodePoint;const aColumn,aRow:TPasTermSizeInt);
       procedure DrawCursor(const aSender:TPasTerm;const aColumn,aRow:TPasTermSizeInt);
      private
       fVulkanGraphicsCommandPool:TpvVulkanCommandPool;
       fVulkanGraphicsCommandBuffer:TpvVulkanCommandBuffer;
       fVulkanGraphicsCommandBufferFence:TpvVulkanFence;
       fVulkanTransferCommandPool:TpvVulkanCommandPool;
       fVulkanTransferCommandBuffer:TpvVulkanCommandBuffer;
       fVulkanTransferCommandBufferFence:TpvVulkanFence;
       fVulkanRenderPass:TpvVulkanRenderPass;
       fVulkanCommandPool:TpvVulkanCommandPool;
       fVulkanRenderCommandBuffers:array[0..MaxInFlightFrames-1] of TpvVulkanCommandBuffer;
       fVulkanRenderSemaphores:array[0..MaxInFlightFrames-1] of TpvVulkanSemaphore;
{      fVulkanSpriteAtlas:TpvSpriteAtlas;
       fVulkanFontSpriteAtlas:TpvSpriteAtlas;}
       fVulkanCanvas:TpvCanvas;
//     fVulkanFont:TpvFont;
       fReady:boolean;
       fLastTerminalMode:Boolean;
       fTerminalMode:Boolean;
       fMouseButtons:TpvUInt32;
       fSelectedIndex:TpvInt32;
       fStartY:TpvFloat;
       fTime:TpvDouble;
       fTerm:TPasTerm;
       fTerminalFrameBufferSnapshot:TPasTerm.TFrameBufferSnapshot;
       fMachineInstance:TMachineInstance;
       fUARTOutputBuffer:array[0..65535] of AnsiChar;
       fContentGeneration:TpVUInt64;
       fRenderGeneration:array[0..MaxInFlightFrames-1] of TpVUInt64;
       fTextureGeneration:TpVUInt64;
      public

       constructor Create; override;

       destructor Destroy; override;

       procedure Show; override;

       procedure Hide; override;

       procedure Resume; override;

       procedure Pause; override;

       procedure Resize(const aWidth,aHeight:TpvInt32); override;

       procedure AfterCreateSwapChain; override;

       procedure BeforeDestroySwapChain; override;

       function KeyEvent(const aKeyEvent:TpvApplicationInputKeyEvent):boolean; override;

       function PointerEvent(const aPointerEvent:TpvApplicationInputPointerEvent):boolean; override;

       function Scrolled(const aRelativeAmount:TpvVector2):boolean; override;

       function CanBeParallelProcessed:boolean; override;

       procedure Check(const aDeltaTime:TpvDouble); override;

       procedure Update(const aDeltaTime:TpvDouble); override;

       procedure Draw(const aSwapChainImageIndex:TpvInt32;var aWaitSemaphore:TpvVulkanSemaphore;const aWaitFence:TpvVulkanFence=nil); override;

     end;

implementation

uses UnitApplication;

function MapKeyCodeToHIDKeyCode(const aKeyCode:TpvUInt32):TpvUInt32;
begin
 case aKeyCode of
  KEYCODE_A:begin
   result:=TPasRISCV.THID.KEY_A;
  end;
  KEYCODE_B:begin
   result:=TPasRISCV.THID.KEY_B;
  end;
  KEYCODE_C:begin
   result:=TPasRISCV.THID.KEY_C;
  end;
  KEYCODE_D:begin
   result:=TPasRISCV.THID.KEY_D;
  end;
  KEYCODE_E:begin
   result:=TPasRISCV.THID.KEY_E;
  end;
  KEYCODE_F:begin
   result:=TPasRISCV.THID.KEY_F;
  end;
  KEYCODE_G:begin
   result:=TPasRISCV.THID.KEY_G;
  end;
  KEYCODE_H:begin
   result:=TPasRISCV.THID.KEY_H;
  end;
  KEYCODE_I:begin
   result:=TPasRISCV.THID.KEY_I;
  end;
  KEYCODE_J:begin
   result:=TPasRISCV.THID.KEY_J;
  end;
  KEYCODE_K:begin
   result:=TPasRISCV.THID.KEY_K;
  end;
  KEYCODE_L:begin
   result:=TPasRISCV.THID.KEY_L;
  end;
  KEYCODE_M:begin
   result:=TPasRISCV.THID.KEY_M;
  end;
  KEYCODE_N:begin
   result:=TPasRISCV.THID.KEY_N;
  end;
  KEYCODE_O:begin
   result:=TPasRISCV.THID.KEY_O;
  end;
  KEYCODE_P:begin
   result:=TPasRISCV.THID.KEY_P;
  end;
  KEYCODE_Q:begin
   result:=TPasRISCV.THID.KEY_Q;
  end;
  KEYCODE_R:begin
   result:=TPasRISCV.THID.KEY_R;
  end;
  KEYCODE_S:begin
   result:=TPasRISCV.THID.KEY_S;
  end;
  KEYCODE_T:begin
   result:=TPasRISCV.THID.KEY_T;
  end;
  KEYCODE_U:begin
   result:=TPasRISCV.THID.KEY_U;
  end;
  KEYCODE_V:begin
   result:=TPasRISCV.THID.KEY_V;
  end;
  KEYCODE_W:begin
   result:=TPasRISCV.THID.KEY_W;
  end;
  KEYCODE_X:begin
   result:=TPasRISCV.THID.KEY_X;
  end;
  KEYCODE_Y:begin
   result:=TPasRISCV.THID.KEY_Y;
  end;
  KEYCODE_Z:begin
   result:=TPasRISCV.THID.KEY_Z;
  end;
  KEYCODE_1:begin
   result:=TPasRISCV.THID.KEY_1;
  end;
  KEYCODE_2:begin
   result:=TPasRISCV.THID.KEY_2;
  end;
  KEYCODE_3:begin
   result:=TPasRISCV.THID.KEY_3;
  end;
  KEYCODE_4:begin
   result:=TPasRISCV.THID.KEY_4;
  end;
  KEYCODE_5:begin
   result:=TPasRISCV.THID.KEY_5;
  end;
  KEYCODE_6:begin
   result:=TPasRISCV.THID.KEY_6;
  end;
  KEYCODE_7:begin
   result:=TPasRISCV.THID.KEY_7;
  end;
  KEYCODE_8:begin
   result:=TPasRISCV.THID.KEY_8;
  end;
  KEYCODE_9:begin
   result:=TPasRISCV.THID.KEY_9;
  end;
  KEYCODE_0:begin
   result:=TPasRISCV.THID.KEY_0;
  end;
  KEYCODE_RETURN:begin
   result:=TPasRISCV.THID.KEY_RETURN;
  end;
  KEYCODE_ESCAPE:begin
   result:=TPasRISCV.THID.KEY_ESCAPE;
  end;
  KEYCODE_BACKSPACE:begin
   result:=TPasRISCV.THID.KEY_BACKSPACE;
  end;
  KEYCODE_TAB:begin
   result:=TPasRISCV.THID.KEY_TAB;
  end;
  KEYCODE_SPACE:begin
   result:=TPasRISCV.THID.KEY_SPACE;
  end;
  KEYCODE_MINUS:begin
   result:=TPasRISCV.THID.KEY_MINUS;
  end;
  KEYCODE_EQUALS:begin
   result:=TPasRISCV.THID.KEY_EQUAL;
  end;
  KEYCODE_LEFTBRACE:begin
   result:=TPasRISCV.THID.KEY_LEFTBRACE;
  end;
  KEYCODE_RIGHTBRACE:begin
   result:=TPasRISCV.THID.KEY_RIGHTBRACE;
  end;
  KEYCODE_BACKSLASH:begin
   result:=TPasRISCV.THID.KEY_BACKSLASH;
  end;
  KEYCODE_TILDE:begin
   result:=TPasRISCV.THID.KEY_HASHTILDE;
  end;
  KEYCODE_SEMICOLON:begin
   result:=TPasRISCV.THID.KEY_SEMICOLON;
  end;
  KEYCODE_APOSTROPHE:begin
   result:=TPasRISCV.THID.KEY_APOSTROPHE;
  end;
  KEYCODE_GRAVE:begin
   result:=TPasRISCV.THID.KEY_GRAVE;
  end;
  KEYCODE_COMMA:begin
   result:=TPasRISCV.THID.KEY_COMMA;
  end;
  KEYCODE_PERIOD:begin
   result:=TPasRISCV.THID.KEY_DOT;
  end;
  KEYCODE_SLASH:begin
   result:=TPasRISCV.THID.KEY_SLASH;
  end;
  KEYCODE_CAPSLOCK:begin
   result:=TPasRISCV.THID.KEY_CAPSLOCK;
  end;
  KEYCODE_F1:begin
   result:=TPasRISCV.THID.KEY_F1;
  end;
  KEYCODE_F2:begin
   result:=TPasRISCV.THID.KEY_F2;
  end;
  KEYCODE_F3:begin
   result:=TPasRISCV.THID.KEY_F3;
  end;
  KEYCODE_F4:begin
   result:=TPasRISCV.THID.KEY_F4;
  end;
  KEYCODE_F5:begin
   result:=TPasRISCV.THID.KEY_F5;
  end;
  KEYCODE_F6:begin
   result:=TPasRISCV.THID.KEY_F6;
  end;
  KEYCODE_F7:begin
   result:=TPasRISCV.THID.KEY_F7;
  end;
  KEYCODE_F8:begin
   result:=TPasRISCV.THID.KEY_F8;
  end;
  KEYCODE_F9:begin
   result:=TPasRISCV.THID.KEY_F9;
  end;
  KEYCODE_F10:begin
   result:=TPasRISCV.THID.KEY_F10;
  end;
  KEYCODE_F11:begin
   result:=TPasRISCV.THID.KEY_F11;
  end;
  KEYCODE_F12:begin
   result:=TPasRISCV.THID.KEY_F12;
  end;
  KEYCODE_SYSREQ:begin
   result:=TPasRISCV.THID.KEY_SYSRQ;
  end;
  KEYCODE_SCROLLLOCK:begin
   result:=TPasRISCV.THID.KEY_SCROLLLOCK;
  end;
  KEYCODE_PAUSE:begin
   result:=TPasRISCV.THID.KEY_PAUSE;
  end;
  KEYCODE_INSERT:begin
   result:=TPasRISCV.THID.KEY_INSERT;
  end;
  KEYCODE_HOME:begin
   result:=TPasRISCV.THID.KEY_HOME;
  end;
  KEYCODE_PAGEUP:begin
   result:=TPasRISCV.THID.KEY_PAGEUP;
  end;
  KEYCODE_DELETE:begin
   result:=TPasRISCV.THID.KEY_DELETE;
  end;
  KEYCODE_END:begin
   result:=TPasRISCV.THID.KEY_END;
  end;
  KEYCODE_PAGEDOWN:begin
   result:=TPasRISCV.THID.KEY_PAGEDOWN;
  end;
  KEYCODE_RIGHT:begin
   result:=TPasRISCV.THID.KEY_RIGHT;
  end;
  KEYCODE_LEFT:begin
   result:=TPasRISCV.THID.KEY_LEFT;
  end;
  KEYCODE_DOWN:begin
   result:=TPasRISCV.THID.KEY_DOWN;
  end;
  KEYCODE_UP:begin
   result:=TPasRISCV.THID.KEY_UP;
  end;
  KEYCODE_NUMLOCK:begin
   result:=TPasRISCV.THID.KEY_NUMLOCK;
  end;
  KEYCODE_KP_DIVIDE:begin
   result:=TPasRISCV.THID.KEY_KPSLASH;
  end;
  KEYCODE_KP_MULTIPLY:begin
   result:=TPasRISCV.THID.KEY_KPASTERISK;
  end;
  KEYCODE_KP_MINUS:begin
   result:=TPasRISCV.THID.KEY_KPMINUS;
  end;
  KEYCODE_KP_PLUS:begin
   result:=TPasRISCV.THID.KEY_KPPLUS;
  end;
  KEYCODE_KP_ENTER:begin
   result:=TPasRISCV.THID.KEY_KPENTER;
  end;
  KEYCODE_KP1:begin
   result:=TPasRISCV.THID.KEY_KP1;
  end;
  KEYCODE_KP2:begin
   result:=TPasRISCV.THID.KEY_KP2;
  end;
  KEYCODE_KP3:begin
   result:=TPasRISCV.THID.KEY_KP3;
  end;
  KEYCODE_KP4:begin
   result:=TPasRISCV.THID.KEY_KP4;
  end;
  KEYCODE_KP5:begin
   result:=TPasRISCV.THID.KEY_KP5;
  end;
  KEYCODE_KP6:begin
   result:=TPasRISCV.THID.KEY_KP6;
  end;
  KEYCODE_KP7:begin
   result:=TPasRISCV.THID.KEY_KP7;
  end;
  KEYCODE_KP8:begin
   result:=TPasRISCV.THID.KEY_KP8;
  end;
  KEYCODE_KP9:begin
   result:=TPasRISCV.THID.KEY_KP9;
  end;
  KEYCODE_KP0:begin
   result:=TPasRISCV.THID.KEY_KP0;
  end;
  KEYCODE_KP_PERIOD:begin
   result:=TPasRISCV.THID.KEY_KPDOT;
  end;
  KEYCODE_102ND:begin
   result:=TPasRISCV.THID.KEY_102ND;
  end;
{ KEYCODE_LMETA:begin
   result:=TPasRISCV.THID.KEY_COMPOSE;
  end;}
  KEYCODE_POWER:begin
   result:=TPasRISCV.THID.KEY_POWER;
  end;
  KEYCODE_KP_EQUALS:begin
   result:=TPasRISCV.THID.KEY_KPEQUAL;
  end;
  KEYCODE_F13:begin
   result:=TPasRISCV.THID.KEY_F13;
  end;
  KEYCODE_F14:begin
   result:=TPasRISCV.THID.KEY_F14;
  end;
  KEYCODE_F15:begin
   result:=TPasRISCV.THID.KEY_F15;
  end;
  KEYCODE_F16:begin
   result:=TPasRISCV.THID.KEY_F16;
  end;
  KEYCODE_F17:begin
   result:=TPasRISCV.THID.KEY_F17;
  end;
  KEYCODE_F18:begin
   result:=TPasRISCV.THID.KEY_F18;
  end;
  KEYCODE_F19:begin
   result:=TPasRISCV.THID.KEY_F19;
  end;
  KEYCODE_F20:begin
   result:=TPasRISCV.THID.KEY_F20;
  end;
  KEYCODE_F21:begin
   result:=TPasRISCV.THID.KEY_F21;
  end;
  KEYCODE_F22:begin
   result:=TPasRISCV.THID.KEY_F22;
  end;
  KEYCODE_F23:begin
   result:=TPasRISCV.THID.KEY_F23;
  end;
  KEYCODE_F24:begin
   result:=TPasRISCV.THID.KEY_F24;
  end;
  KEYCODE_AC_BOOKMARKS:begin
   result:=TPasRISCV.THID.KEY_OPEN;
  end;
  KEYCODE_HELP:begin
   result:=TPasRISCV.THID.KEY_HELP;
  end;
  KEYCODE_MENU:begin
   result:=TPasRISCV.THID.KEY_MENU;
  end;
{ KEYCODE_FRONT:begin
   result:=TPasRISCV.THID.KEY_FRONT;
  end;}
  KEYCODE_STOP:begin
   result:=TPasRISCV.THID.KEY_STOP;
  end;
  KEYCODE_AGAIN:begin
   result:=TPasRISCV.THID.KEY_AGAIN;
  end;
  KEYCODE_UNDO:begin
   result:=TPasRISCV.THID.KEY_UNDO;
  end;
  KEYCODE_CUT:begin
   result:=TPasRISCV.THID.KEY_CUT;
  end;
  KEYCODE_COPY:begin
   result:=TPasRISCV.THID.KEY_COPY;
  end;
  KEYCODE_PASTE:begin
   result:=TPasRISCV.THID.KEY_PASTE;
  end;
  KEYCODE_FIND:begin
   result:=TPasRISCV.THID.KEY_FIND;
  end;
  KEYCODE_MUTE:begin
   result:=TPasRISCV.THID.KEY_MUTE;
  end;
  KEYCODE_VOLUMEUP:begin
   result:=TPasRISCV.THID.KEY_VOLUMEUP;
  end;
  KEYCODE_VOLUMEDOWN:begin
   result:=TPasRISCV.THID.KEY_VOLUMEDOWN;
  end;
  KEYCODE_KP_COMMA:begin
   result:=TPasRISCV.THID.KEY_KPCOMMA;
  end;
{KEYCODE_RO:begin
   result:=TPasRISCV.THID.KEY_RO;
  end;}
  KEYCODE_KATAKANAHIRAGANA:begin
   result:=TPasRISCV.THID.KEY_KATAKANAHIRAGANA;
  end;
{ KEYCODE_YEN:begin
   result:=TPasRISCV.THID.KEY_YEN;
  end;}
  KEYCODE_HENKAN:begin
   result:=TPasRISCV.THID.KEY_HENKAN;
  end;
  KEYCODE_MUHENKAN:begin
   result:=TPasRISCV.THID.KEY_MUHENKAN;
  end;
{ KEYCODE_KPJPCOMMA:begin
   result:=TPasRISCV.THID.KEY_KPJPCOMMA;
  end}
  KEYCODE_HANGEUL:begin
   result:=TPasRISCV.THID.KEY_HANGEUL;
  end;
  KEYCODE_HANJA:begin
   result:=TPasRISCV.THID.KEY_HANJA;
  end;
{ KEYCODE_KATAKANA:begin
   result:=TPasRISCV.THID.KEY_KATAKANA;
  end;
  KEYCODE_HIRAGANA:begin
   result:=TPasRISCV.THID.KEY_HIRAGANA;
  end
  KEYCODE_ZENKAKUHANKAKU:begin
   result:=TPasRISCV.THID.KEY_ZENKAKUHANKAKU;
  end;}
  KEYCODE_KP_LEFTPAREN:begin
   result:=TPasRISCV.THID.KEY_KPLEFTPAREN;
  end;
  KEYCODE_KP_RIGHTPAREN:begin
   result:=TPasRISCV.THID.KEY_KPRIGHTPAREN;
  end;
  KEYCODE_LCTRL:begin
   result:=TPasRISCV.THID.KEY_LEFTCTRL;
  end;
  KEYCODE_LSHIFT:begin
   result:=TPasRISCV.THID.KEY_LEFTSHIFT;
  end;
  KEYCODE_LALT:begin
   result:=TPasRISCV.THID.KEY_LEFTALT;
  end;
  KEYCODE_LGUI:begin
   result:=TPasRISCV.THID.KEY_LEFTMETA;
  end;
  KEYCODE_RCTRL:begin
   result:=TPasRISCV.THID.KEY_RIGHTCTRL;
  end;
  KEYCODE_RSHIFT:begin
   result:=TPasRISCV.THID.KEY_RIGHTSHIFT;
  end;
  KEYCODE_RALT:begin
   result:=TPasRISCV.THID.KEY_RIGHTALT;
  end;
  KEYCODE_RGUI:begin
   result:=TPasRISCV.THID.KEY_RIGHTMETA;
  end;
  KEYCODE_AUDIOPLAY:begin
   result:=TPasRISCV.THID.KEY_MEDIA_PLAYPAUSE;
  end;
  KEYCODE_AC_STOP:begin
   result:=TPasRISCV.THID.KEY_MEDIA_STOPCD;
  end;
  KEYCODE_AUDIOPREV:begin
   result:=TPasRISCV.THID.KEY_MEDIA_PREVIOUSSONG;
  end;
  KEYCODE_AUDIONEXT:begin
   result:=TPasRISCV.THID.KEY_MEDIA_NEXTSONG;
  end;
  KEYCODE_EJECT:begin
   result:=TPasRISCV.THID.KEY_MEDIA_EJECTCD;
  end;
{KEYCODE_VOLUMEUP:begin
   result:=TPasRISCV.THID.KEY_MEDIA_VOLUMEUP;
  end;
  KEYCODE_VOLUMEDOWN:begin
   result:=TPasRISCV.THID.KEY_MEDIA_VOLUMEDOWN;
  end;}
{ KEYCODE_MUTE:begin
   result:=TPasRISCV.THID.KEY_MEDIA_MUTE;
  end;}
  KEYCODE_WWW:begin
   result:=TPasRISCV.THID.KEY_MEDIA_WWW;
  end;
  KEYCODE_AC_BACK:begin
   result:=TPasRISCV.THID.KEY_MEDIA_BACK;
  end;
  KEYCODE_AC_FORWARD:begin
   result:=TPasRISCV.THID.KEY_MEDIA_FORWARD;
  end;
  KEYCODE_AUDIOSTOP:begin
   result:=TPasRISCV.THID.KEY_MEDIA_STOP;
  end;
  KEYCODE_AC_SEARCH:begin
   result:=TPasRISCV.THID.KEY_MEDIA_FIND;
  end;
{ KEYCODE_MEDIA_SCROLLUP:begin
   result:=TPasRISCV.THID.KEY_MEDIA_SCROLLUP;
  end;
  KEYCODE_MEDIA_SCROLLDOWN:begin
   result:=TPasRISCV.THID.KEY_MEDIA_SCROLLDOWN;
  end;}
  KEYCODE_MEDIASELECT:begin
   result:=TPasRISCV.THID.KEY_MEDIA_EDIT;
  end;
  KEYCODE_SLEEP:begin
   result:=TPasRISCV.THID.KEY_MEDIA_SLEEP;
  end;
{ KEYCODE_AC_COFFEE:begin
   result:=TPasRISCV.THID.KEY_MEDIA_COFFEE;
  end;}
  KEYCODE_AC_REFRESH:begin
   result:=TPasRISCV.THID.KEY_MEDIA_REFRESH;
  end;
  KEYCODE_CALCULATOR:begin
   result:=TPasRISCV.THID.KEY_MEDIA_CALC;
  end;
  else begin
   result:=TPasRISCV.THID.KEY_NONE;
  end;
 end;
end;

function MapKeyCodeToEVDEVKeyCode(const aKeyCode:TpvUInt32):TpvUInt32;
begin
 case aKeyCode of
  KEYCODE_A:begin
   result:=TPasRISCV.TEVDEV.KEY_A;
  end;
  KEYCODE_B:begin
   result:=TPasRISCV.TEVDEV.KEY_B;
  end;
  KEYCODE_C:begin
   result:=TPasRISCV.TEVDEV.KEY_C;
  end;
  KEYCODE_D:begin
   result:=TPasRISCV.TEVDEV.KEY_D;
  end;
  KEYCODE_E:begin
   result:=TPasRISCV.TEVDEV.KEY_E;
  end;
  KEYCODE_F:begin
   result:=TPasRISCV.TEVDEV.KEY_F;
  end;
  KEYCODE_G:begin
   result:=TPasRISCV.TEVDEV.KEY_G;
  end;
  KEYCODE_H:begin
   result:=TPasRISCV.TEVDEV.KEY_H;
  end;
  KEYCODE_I:begin
   result:=TPasRISCV.TEVDEV.KEY_I;
  end;
  KEYCODE_J:begin
   result:=TPasRISCV.TEVDEV.KEY_J;
  end;
  KEYCODE_K:begin
   result:=TPasRISCV.TEVDEV.KEY_K;
  end;
  KEYCODE_L:begin
   result:=TPasRISCV.TEVDEV.KEY_L;
  end;
  KEYCODE_M:begin
   result:=TPasRISCV.TEVDEV.KEY_M;
  end;
  KEYCODE_N:begin
   result:=TPasRISCV.TEVDEV.KEY_N;
  end;
  KEYCODE_O:begin
   result:=TPasRISCV.TEVDEV.KEY_O;
  end;
  KEYCODE_P:begin
   result:=TPasRISCV.TEVDEV.KEY_P;
  end;
  KEYCODE_Q:begin
   result:=TPasRISCV.TEVDEV.KEY_Q;
  end;
  KEYCODE_R:begin
   result:=TPasRISCV.TEVDEV.KEY_R;
  end;
  KEYCODE_S:begin
   result:=TPasRISCV.TEVDEV.KEY_S;
  end;
  KEYCODE_T:begin
   result:=TPasRISCV.TEVDEV.KEY_T;
  end;
  KEYCODE_U:begin
   result:=TPasRISCV.TEVDEV.KEY_U;
  end;
  KEYCODE_V:begin
   result:=TPasRISCV.TEVDEV.KEY_V;
  end;
  KEYCODE_W:begin
   result:=TPasRISCV.TEVDEV.KEY_W;
  end;
  KEYCODE_X:begin
   result:=TPasRISCV.TEVDEV.KEY_X;
  end;
  KEYCODE_Y:begin
   result:=TPasRISCV.TEVDEV.KEY_Y;
  end;
  KEYCODE_Z:begin
   result:=TPasRISCV.TEVDEV.KEY_Z;
  end;
  KEYCODE_1:begin
   result:=TPasRISCV.TEVDEV.KEY_1;
  end;
  KEYCODE_2:begin
   result:=TPasRISCV.TEVDEV.KEY_2;
  end;
  KEYCODE_3:begin
   result:=TPasRISCV.TEVDEV.KEY_3;
  end;
  KEYCODE_4:begin
   result:=TPasRISCV.TEVDEV.KEY_4;
  end;
  KEYCODE_5:begin
   result:=TPasRISCV.TEVDEV.KEY_5;
  end;
  KEYCODE_6:begin
   result:=TPasRISCV.TEVDEV.KEY_6;
  end;
  KEYCODE_7:begin
   result:=TPasRISCV.TEVDEV.KEY_7;
  end;
  KEYCODE_8:begin
   result:=TPasRISCV.TEVDEV.KEY_8;
  end;
  KEYCODE_9:begin
   result:=TPasRISCV.TEVDEV.KEY_9;
  end;
  KEYCODE_0:begin
   result:=TPasRISCV.TEVDEV.KEY_0;
  end;
  KEYCODE_RETURN:begin
   result:=TPasRISCV.TEVDEV.KEY_ENTER;
  end;
  KEYCODE_ESCAPE:begin
   result:=TPasRISCV.TEVDEV.KEY_ESC;
  end;
  KEYCODE_BACKSPACE:begin
   result:=TPasRISCV.TEVDEV.KEY_BACKSPACE;
  end;
  KEYCODE_TAB:begin
   result:=TPasRISCV.TEVDEV.KEY_TAB;
  end;
  KEYCODE_SPACE:begin
   result:=TPasRISCV.TEVDEV.KEY_SPACE;
  end;
  KEYCODE_MINUS:begin
   result:=TPasRISCV.TEVDEV.KEY_MINUS;
  end;
  KEYCODE_EQUALS:begin
   result:=TPasRISCV.TEVDEV.KEY_EQUAL;
  end;
  KEYCODE_LEFTBRACKET,KEYCODE_LEFTBRACE:begin
   result:=TPasRISCV.TEVDEV.KEY_LEFTBRACE;
  end;
  KEYCODE_RIGHTBRACKET,KEYCODE_RIGHTBRACE:begin
   result:=TPasRISCV.TEVDEV.KEY_RIGHTBRACE;
  end;
  KEYCODE_BACKSLASH:begin
   result:=TPasRISCV.TEVDEV.KEY_BACKSLASH;
  end;
{ KEYCODE_TILDE:begin
   result:=TPasRISCV.TEVDEV.KEY_HASHTILDE;
  end;}
  KEYCODE_SEMICOLON:begin
   result:=TPasRISCV.TEVDEV.KEY_SEMICOLON;
  end;
  KEYCODE_APOSTROPHE:begin
   result:=TPasRISCV.TEVDEV.KEY_APOSTROPHE;
  end;
  KEYCODE_GRAVE:begin
   result:=TPasRISCV.TEVDEV.KEY_GRAVE;
  end;
  KEYCODE_COMMA:begin
   result:=TPasRISCV.TEVDEV.KEY_COMMA;
  end;
  KEYCODE_PERIOD:begin
   result:=TPasRISCV.TEVDEV.KEY_DOT;
  end;
  KEYCODE_SLASH:begin
   result:=TPasRISCV.TEVDEV.KEY_SLASH;
  end;
  KEYCODE_CAPSLOCK:begin
   result:=TPasRISCV.TEVDEV.KEY_CAPSLOCK;
  end;
  KEYCODE_F1:begin
   result:=TPasRISCV.TEVDEV.KEY_F1;
  end;
  KEYCODE_F2:begin
   result:=TPasRISCV.TEVDEV.KEY_F2;
  end;
  KEYCODE_F3:begin
   result:=TPasRISCV.TEVDEV.KEY_F3;
  end;
  KEYCODE_F4:begin
   result:=TPasRISCV.TEVDEV.KEY_F4;
  end;
  KEYCODE_F5:begin
   result:=TPasRISCV.TEVDEV.KEY_F5;
  end;
  KEYCODE_F6:begin
   result:=TPasRISCV.TEVDEV.KEY_F6;
  end;
  KEYCODE_F7:begin
   result:=TPasRISCV.TEVDEV.KEY_F7;
  end;
  KEYCODE_F8:begin
   result:=TPasRISCV.TEVDEV.KEY_F8;
  end;
  KEYCODE_F9:begin
   result:=TPasRISCV.TEVDEV.KEY_F9;
  end;
  KEYCODE_F10:begin
   result:=TPasRISCV.TEVDEV.KEY_F10;
  end;
  KEYCODE_F11:begin
   result:=TPasRISCV.TEVDEV.KEY_F11;
  end;
  KEYCODE_F12:begin
   result:=TPasRISCV.TEVDEV.KEY_F12;
  end;
  KEYCODE_SYSREQ:begin
   result:=TPasRISCV.TEVDEV.KEY_SYSRQ;
  end;
  KEYCODE_SCROLLLOCK:begin
   result:=TPasRISCV.TEVDEV.KEY_SCROLLLOCK;
  end;
  KEYCODE_PAUSE:begin
   result:=TPasRISCV.TEVDEV.KEY_PAUSE;
  end;
  KEYCODE_INSERT:begin
   result:=TPasRISCV.TEVDEV.KEY_INSERT;
  end;
  KEYCODE_HOME:begin
   result:=TPasRISCV.TEVDEV.KEY_HOME;
  end;
  KEYCODE_PAGEUP:begin
   result:=TPasRISCV.TEVDEV.KEY_PAGEUP;
  end;
  KEYCODE_DELETE:begin
   result:=TPasRISCV.TEVDEV.KEY_DELETE;
  end;
  KEYCODE_END:begin
   result:=TPasRISCV.TEVDEV.KEY_END;
  end;
  KEYCODE_PAGEDOWN:begin
   result:=TPasRISCV.TEVDEV.KEY_PAGEDOWN;
  end;
  KEYCODE_RIGHT:begin
   result:=TPasRISCV.TEVDEV.KEY_RIGHT;
  end;
  KEYCODE_LEFT:begin
   result:=TPasRISCV.TEVDEV.KEY_LEFT;
  end;
  KEYCODE_DOWN:begin
   result:=TPasRISCV.TEVDEV.KEY_DOWN;
  end;
  KEYCODE_UP:begin
   result:=TPasRISCV.TEVDEV.KEY_UP;
  end;
  KEYCODE_NUMLOCK:begin
   result:=TPasRISCV.TEVDEV.KEY_NUMLOCK;
  end;
  KEYCODE_KP_DIVIDE:begin
   result:=TPasRISCV.TEVDEV.KEY_KPSLASH;
  end;
  KEYCODE_KP_MULTIPLY:begin
   result:=TPasRISCV.TEVDEV.KEY_KPASTERISK;
  end;
  KEYCODE_KP_MINUS:begin
   result:=TPasRISCV.TEVDEV.KEY_KPMINUS;
  end;
  KEYCODE_KP_PLUS:begin
   result:=TPasRISCV.TEVDEV.KEY_KPPLUS;
  end;
  KEYCODE_KP_ENTER:begin
   result:=TPasRISCV.TEVDEV.KEY_KPENTER;
  end;
  KEYCODE_KP1:begin
   result:=TPasRISCV.TEVDEV.KEY_KP1;
  end;
  KEYCODE_KP2:begin
   result:=TPasRISCV.TEVDEV.KEY_KP2;
  end;
  KEYCODE_KP3:begin
   result:=TPasRISCV.TEVDEV.KEY_KP3;
  end;
  KEYCODE_KP4:begin
   result:=TPasRISCV.TEVDEV.KEY_KP4;
  end;
  KEYCODE_KP5:begin
   result:=TPasRISCV.TEVDEV.KEY_KP5;
  end;
  KEYCODE_KP6:begin
   result:=TPasRISCV.TEVDEV.KEY_KP6;
  end;
  KEYCODE_KP7:begin
   result:=TPasRISCV.TEVDEV.KEY_KP7;
  end;
  KEYCODE_KP8:begin
   result:=TPasRISCV.TEVDEV.KEY_KP8;
  end;
  KEYCODE_KP9:begin
   result:=TPasRISCV.TEVDEV.KEY_KP9;
  end;
  KEYCODE_KP0:begin
   result:=TPasRISCV.TEVDEV.KEY_KP0;
  end;
  KEYCODE_KP_PERIOD:begin
   result:=TPasRISCV.TEVDEV.KEY_KPDOT;
  end;
  KEYCODE_102ND:begin
   result:=TPasRISCV.TEVDEV.KEY_102ND;
  end;
{ KEYCODE_LMETA:begin
   result:=TPasRISCV.TEVDEV.KEY_COMPOSE;
  end;}
  KEYCODE_POWER:begin
   result:=TPasRISCV.TEVDEV.KEY_POWER;
  end;
  KEYCODE_KP_EQUALS:begin
   result:=TPasRISCV.TEVDEV.KEY_KPEQUAL;
  end;
  KEYCODE_F13:begin
   result:=TPasRISCV.TEVDEV.KEY_F13;
  end;
  KEYCODE_F14:begin
   result:=TPasRISCV.TEVDEV.KEY_F14;
  end;
  KEYCODE_F15:begin
   result:=TPasRISCV.TEVDEV.KEY_F15;
  end;
  KEYCODE_F16:begin
   result:=TPasRISCV.TEVDEV.KEY_F16;
  end;
  KEYCODE_F17:begin
   result:=TPasRISCV.TEVDEV.KEY_F17;
  end;
  KEYCODE_F18:begin
   result:=TPasRISCV.TEVDEV.KEY_F18;
  end;
  KEYCODE_F19:begin
   result:=TPasRISCV.TEVDEV.KEY_F19;
  end;
  KEYCODE_F20:begin
   result:=TPasRISCV.TEVDEV.KEY_F20;
  end;
  KEYCODE_F21:begin
   result:=TPasRISCV.TEVDEV.KEY_F21;
  end;
  KEYCODE_F22:begin
   result:=TPasRISCV.TEVDEV.KEY_F22;
  end;
  KEYCODE_F23:begin
   result:=TPasRISCV.TEVDEV.KEY_F23;
  end;
  KEYCODE_F24:begin
   result:=TPasRISCV.TEVDEV.KEY_F24;
  end;
  KEYCODE_AC_BOOKMARKS:begin
   result:=TPasRISCV.TEVDEV.KEY_OPEN;
  end;
  KEYCODE_HELP:begin
   result:=TPasRISCV.TEVDEV.KEY_HELP;
  end;
  KEYCODE_MENU:begin
   result:=TPasRISCV.TEVDEV.KEY_MENU;
  end;
{ KEYCODE_FRONT:begin
   result:=TPasRISCV.TEVDEV.KEY_FRONT;
  end;}
  KEYCODE_STOP:begin
   result:=TPasRISCV.TEVDEV.KEY_STOP;
  end;
  KEYCODE_AGAIN:begin
   result:=TPasRISCV.TEVDEV.KEY_AGAIN;
  end;
  KEYCODE_UNDO:begin
   result:=TPasRISCV.TEVDEV.KEY_UNDO;
  end;
  KEYCODE_CUT:begin
   result:=TPasRISCV.TEVDEV.KEY_CUT;
  end;
  KEYCODE_COPY:begin
   result:=TPasRISCV.TEVDEV.KEY_COPY;
  end;
  KEYCODE_PASTE:begin
   result:=TPasRISCV.TEVDEV.KEY_PASTE;
  end;
  KEYCODE_FIND:begin
   result:=TPasRISCV.TEVDEV.KEY_FIND;
  end;
  KEYCODE_MUTE:begin
   result:=TPasRISCV.TEVDEV.KEY_MUTE;
  end;
  KEYCODE_VOLUMEUP:begin
   result:=TPasRISCV.TEVDEV.KEY_VOLUMEUP;
  end;
  KEYCODE_VOLUMEDOWN:begin
   result:=TPasRISCV.TEVDEV.KEY_VOLUMEDOWN;
  end;
  KEYCODE_KP_COMMA:begin
   result:=TPasRISCV.TEVDEV.KEY_KPCOMMA;
  end;
{KEYCODE_RO:begin
   result:=TPasRISCV.TEVDEV.KEY_RO;
  end;}
  KEYCODE_KATAKANAHIRAGANA:begin
   result:=TPasRISCV.TEVDEV.KEY_KATAKANAHIRAGANA;
  end;
{ KEYCODE_YEN:begin
   result:=TPasRISCV.TEVDEV.KEY_YEN;
  end;}
  KEYCODE_HENKAN:begin
   result:=TPasRISCV.TEVDEV.KEY_HENKAN;
  end;
  KEYCODE_MUHENKAN:begin
   result:=TPasRISCV.TEVDEV.KEY_MUHENKAN;
  end;
{ KEYCODE_KPJPCOMMA:begin
   result:=TPasRISCV.TEVDEV.KEY_KPJPCOMMA;
  end}
  KEYCODE_HANGEUL:begin
   result:=TPasRISCV.TEVDEV.KEY_HANGEUL;
  end;
  KEYCODE_HANJA:begin
   result:=TPasRISCV.TEVDEV.KEY_HANJA;
  end;
{ KEYCODE_KATAKANA:begin
   result:=TPasRISCV.TEVDEV.KEY_KATAKANA;
  end;
  KEYCODE_HIRAGANA:begin
   result:=TPasRISCV.TEVDEV.KEY_HIRAGANA;
  end
  KEYCODE_ZENKAKUHANKAKU:begin
   result:=TPasRISCV.TEVDEV.KEY_ZENKAKUHANKAKU;
  end;}
  KEYCODE_KP_LEFTPAREN:begin
   result:=TPasRISCV.TEVDEV.KEY_KPLEFTPAREN;
  end;
  KEYCODE_KP_RIGHTPAREN:begin
   result:=TPasRISCV.TEVDEV.KEY_KPRIGHTPAREN;
  end;
  KEYCODE_LCTRL:begin
   result:=TPasRISCV.TEVDEV.KEY_LEFTCTRL;
  end;
  KEYCODE_LSHIFT:begin
   result:=TPasRISCV.TEVDEV.KEY_LEFTSHIFT;
  end;
  KEYCODE_LALT:begin
   result:=TPasRISCV.TEVDEV.KEY_LEFTALT;
  end;
  KEYCODE_LGUI:begin
   result:=TPasRISCV.TEVDEV.KEY_LEFTMETA;
  end;
  KEYCODE_RCTRL:begin
   result:=TPasRISCV.TEVDEV.KEY_RIGHTCTRL;
  end;
  KEYCODE_RSHIFT:begin
   result:=TPasRISCV.TEVDEV.KEY_RIGHTSHIFT;
  end;
  KEYCODE_RALT:begin
   result:=TPasRISCV.TEVDEV.KEY_RIGHTALT;
  end;
  KEYCODE_RGUI:begin
   result:=TPasRISCV.TEVDEV.KEY_RIGHTMETA;
  end;
  KEYCODE_AUDIOPLAY:begin
   result:=TPasRISCV.TEVDEV.KEY_PLAY;
  end;
  KEYCODE_AC_STOP:begin
   result:=TPasRISCV.TEVDEV.KEY_STOP;
  end;
  KEYCODE_AUDIOPREV:begin
   result:=TPasRISCV.TEVDEV.KEY_PREVIOUSSONG;
  end;
  KEYCODE_AUDIONEXT:begin
   result:=TPasRISCV.TEVDEV.KEY_NEXTSONG;
  end;
  KEYCODE_EJECT:begin
   result:=TPasRISCV.TEVDEV.KEY_EJECTCD;
  end;
{KEYCODE_VOLUMEUP:begin
   result:=TPasRISCV.TEVDEV.KEY_MEDIA_VOLUMEUP;
  end;
  KEYCODE_VOLUMEDOWN:begin
   result:=TPasRISCV.TEVDEV.KEY_MEDIA_VOLUMEDOWN;
  end;}
{ KEYCODE_MUTE:begin
   result:=TPasRISCV.TEVDEV.KEY_MEDIA_MUTE;
  end;}
  KEYCODE_WWW:begin
   result:=TPasRISCV.TEVDEV.KEY_WWW;
  end;
  KEYCODE_AC_BACK:begin
   result:=TPasRISCV.TEVDEV.KEY_BACK;
  end;
  KEYCODE_AC_FORWARD:begin
   result:=TPasRISCV.TEVDEV.KEY_FORWARD;
  end;
  KEYCODE_AUDIOSTOP:begin
   result:=TPasRISCV.TEVDEV.KEY_STOP;
  end;
  KEYCODE_AC_SEARCH:begin
   result:=TPasRISCV.TEVDEV.KEY_SEARCH;
  end;
{ KEYCODE_MEDIA_SCROLLUP:begin
   result:=TPasRISCV.TEVDEV.KEY_MEDIA_SCROLLUP;
  end;
  KEYCODE_MEDIA_SCROLLDOWN:begin
   result:=TPasRISCV.TEVDEV.KEY_MEDIA_SCROLLDOWN;
  end;}
  KEYCODE_MEDIASELECT:begin
   result:=TPasRISCV.TEVDEV.KEY_MEDIA;
  end;
  KEYCODE_SLEEP:begin
   result:=TPasRISCV.TEVDEV.KEY_SLEEP;
  end;
{ KEYCODE_AC_COFFEE:begin
   result:=TPasRISCV.TEVDEV.KEY_MEDIA_COFFEE;
  end;}
  KEYCODE_AC_REFRESH:begin
   result:=TPasRISCV.TEVDEV.KEY_REFRESH;
  end;
  KEYCODE_CALCULATOR:begin
   result:=TPasRISCV.TEVDEV.KEY_CALC;
  end;
  else begin
   result:=TPasRISCV.TEVDEV.KEY_UNKNOWN;
  end;
 end;
end;

procedure ResizeRGB32(Src:pointer;SrcWidth,SrcHeight:TpvInt32;Dst:pointer;DstWidth,DstHeight:TpvInt32;var XCache:TMachineInstance.TIntegers);
type PLongwords=^TLongwords;
     TLongwords=array[0..65535] of TpvUInt32;
var DstX,DstY,SrcX,SrcY:TpvInt32;
    r,g,b,w,Pixel,SrcR,SrcG,SrcB,SrcA,Weight,xUL,xUR,xLL,xLR,
    RedBlue,Green,Remainder,WeightX,WeightY:TpvUInt32;
    TempSrc,TempDst:PLongwords;
    UpsampleX,UpsampleY:longbool;
    WeightShift,xa,xb,xc,xd,ya,yb,yc,yd:TpvInt32;
    SourceTexelsPerOutPixel,WeightPerPixel,AccumlatorPerPixel,WeightDivider,fw,fh:single;
begin
 if (SrcWidth=(DstWidth*2)) and (SrcHeight=(DstHeight*2)) then begin
  Remainder:=0;
  TempDst:=pointer(Dst);
  for DstY:=0 to DstHeight-1 do begin
   SrcY:=DstY*2;
   TempSrc:=pointer(@pansichar(Src)[(SrcY*SrcWidth) shl 2]);
   for DstX:=0 to DstWidth-1 do begin
    xUL:=TempSrc^[0];
    xUR:=TempSrc^[1];
    xLL:=TempSrc^[SrcWidth];
    xLR:=TempSrc^[SrcWidth+1];
    RedBlue:=(xUL and $00ff00ff)+(xUR and $00ff00ff)+(xLL and $00ff00ff)+(xLR and $00ff00ff)+(Remainder and $00ff00ff);
    Green:=(xUL and $0000ff00)+(xUR and $0000ff00)+(xLL and $0000ff00)+(xLR and $0000ff00)+(Remainder and $0000ff00);
    Remainder:=(RedBlue and $00030003) or (Green and $00000300);
    TempDst[0]:=(((RedBlue and $03fc03fc) or (Green and $0003fc00)) shr 2) or TpvUInt32($ff000000);
    TempDst:=pointer(@TempDst^[1]);
    TempSrc:=pointer(@TempSrc^[2]);
   end;
  end;
 end else begin
  UpsampleX:=SrcWidth<DstWidth;
  UpsampleY:=DstHeight<DstHeight;
  WeightShift:=0;
  SourceTexelsPerOutPixel:=((SrcWidth/DstWidth)+1)*((SrcHeight/DstHeight)+1);
  WeightPerPixel:=SourceTexelsPerOutPixel*65536;
  AccumlatorPerPixel:=WeightPerPixel*256;
  WeightDivider:=AccumlatorPerPixel/4294967000.0;
  if WeightDivider>1.0 then begin
   WeightShift:=trunc(ceil(ln(WeightDivider)/ln(2.0)));
  end;
  WeightShift:=min(WeightShift,15);
  fw:=(256*SrcWidth)/DstWidth;
  fh:=(256*SrcHeight)/DstHeight;
  if UpsampleX and UpsampleY then begin
   if length(XCache)<TpvInt32(DstWidth) then begin
    SetLength(XCache,TpvInt32(DstWidth));
   end;
   for DstX:=0 to DstWidth-1 do begin
    XCache[DstX]:=min(trunc(DstX*fw),(256*(SrcWidth-1))-1);
   end;
   for DstY:=0 to DstHeight-1 do begin
    ya:=min(trunc(DstY*fh),(256*(SrcHeight-1))-1);
    yc:=ya shr 8;
    TempDst:=pointer(@pansichar(Dst)[(DstY*DstWidth) shl 2]);
    for DstX:=0 to DstWidth-1 do begin
     xa:=XCache[DstX];
     xc:=xa shr 8;
     TempSrc:=pointer(@pansichar(Src)[((yc*SrcWidth)+xc) shl 2]);
     r:=0;
     g:=0;
     b:=0;
     WeightX:=TpvUInt32(TpvInt32(256-(xa and $ff)));
     WeightY:=TpvUInt32(TpvInt32(256-(ya and $ff)));
     for SrcY:=0 to 1 do begin
      for SrcX:=0 to 1 do begin
       Pixel:=TempSrc^[(SrcY*SrcWidth)+SrcX];
       SrcR:=(Pixel shr 0) and $ff;
       SrcG:=(Pixel shr 8) and $ff;
       SrcB:=(Pixel shr 16) and $ff;
       Weight:=(WeightX*WeightY) shr WeightShift;
       inc(r,SrcR*Weight);
       inc(g,SrcG*Weight);
       inc(b,SrcB*Weight);
       WeightX:=256-WeightX;
      end;
      WeightY:=256-WeightY;
     end;
     TempDst^[0]:=(((r shr 16) and $ff) or ((g shr 8) and $ff00) or (b and $ff0000)) or TpvUInt32($ff000000);
     TempDst:=pointer(@TempDst^[1]);
    end;
   end;
  end else begin
   if length(XCache)<(TpvInt32(DstWidth)*2) then begin
    SetLength(XCache,TpvInt32(DstWidth)*2);
   end;
   for DstX:=0 to DstWidth-1 do begin
    xa:=trunc(DstX*fw);
    if UpsampleX then begin
     xb:=xa+256;
    end else begin
     xb:=trunc((DstX+1)*fw);
    end;
    XCache[(DstX shl 1) or 0]:=min(xa,(256*SrcWidth)-1);
    XCache[(DstX shl 1) or 1]:=min(xb,(256*SrcWidth)-1);
   end;
   for DstY:=0 to DstHeight-1 do begin
    ya:=trunc(DstY*fh);
    if UpsampleY then begin
     yb:=ya+256;
    end else begin
     yb:=trunc((DstY+1)*fh);
    end;
    TempDst:=pointer(@pansichar(Dst)[(DstY*DstWidth) shl 2]);
    yc:=ya shr 8;
    yd:=yb shr 8;
    for DstX:=0 to DstWidth-1 do begin
     xa:=XCache[(DstX shl 1) or 0];
     xb:=XCache[(DstX shl 1) or 1];
     xc:=xa shr 8;
     xd:=xb shr 8;
     r:=0;
     g:=0;
     b:=0;
     w:=0;
     for SrcY:=yc to yd do begin
      if (SrcY<0) or (SrcY>=SrcHeight) then begin
       continue;
      end;
      WeightY:=256;
      if yc<>yd then begin
       if SrcY=yc then begin
        WeightY:=256-(ya and $ff);
       end else if SrcY=yd then begin
        WeightY:=yb and $ff;
       end;
      end;
      TempSrc:=pointer(@pansichar(Src)[((SrcY*SrcWidth)+xc) shl 2]);
      for SrcX:=xc to xd do begin
       if (SrcX<0) or (SrcX>=SrcWidth) then begin
        continue;
       end;
       WeightX:=256;
       if xc<>xd then begin
        if SrcX=xc then begin
         WeightX:=256-(xa and $ff);
        end else if SrcX=xd then begin
         WeightX:=xb and $ff;
        end;
       end;
       Pixel:=TempSrc^[0];
       inc(PAnsiChar(TempSrc),SizeOf(TpvUInt32));
       SrcR:=(Pixel shr 0) and $ff;
       SrcG:=(Pixel shr 8) and $ff;
       SrcB:=(Pixel shr 16) and $ff;
       Weight:=(WeightX*WeightY) shr WeightShift;
       inc(r,SrcR*Weight);
       inc(g,SrcG*Weight);
       inc(b,SrcB*Weight);
       inc(w,Weight);
      end;
     end;
     if w>0 then begin
      TempDst^[0]:=(((r div w) and $ff) or (((g div w) shl 8) and $ff00) or (((b div w) shl 16) and $ff0000)) or TpvUInt32($ff000000);
     end else begin
      TempDst^[0]:=TpvUInt32($ff000000);
     end;
     TempDst:=pointer(@TempDst^[1]);
    end;
   end;
  end;
 end;
end;

{ TMachineInstance }

constructor TMachineInstance.Create;
var Stream:TStream;
begin

 fXCacheIntegers:=nil;

 fMachineConfiguration:=TPasRISCV.TConfiguration.Create;

 fMachineConfiguration.Debugger:=Application.DebuggerPort>=0;

 fMachineConfiguration.DebuggerPort:=Application.DebuggerPort;

 fMachineConfiguration.CountCPUCores:=Application.CountCPUCores;

 fMachineConfiguration.BootArguments:=Application.BootArguments;

 fMachineConfiguration.MemorySize:=TPasRISCVUInt64(2048) shl 20;

 fNextFrameTime:=0;

 if FileExists(Application.BIOSFileName) then begin
  fMachineConfiguration.LoadBIOSFromFile(Application.BIOSFileName);
 end else if pvApplication.Assets.ExistAsset('riscv/'+Application.BIOSFileName) then begin
  Stream:=pvApplication.Assets.GetAssetStream('riscv/'+Application.BIOSFileName);
  if assigned(Stream) then begin
   try
    fMachineConfiguration.LoadBIOSFromStream(Stream);
   finally
    FreeAndNil(Stream);
   end;
  end;
 end;

 if FileExists(Application.KernelFileName) then begin
  fMachineConfiguration.LoadKernelFromFile(Application.KernelFileName);
 end else if pvApplication.Assets.ExistAsset('riscv/'+Application.KernelFileName) then begin
  Stream:=pvApplication.Assets.GetAssetStream('riscv/'+Application.KernelFileName);
  if assigned(Stream) then begin
   try
    fMachineConfiguration.LoadKernelFromStream(Stream);
   finally
    FreeAndNil(Stream);
   end;
  end;
 end;

 if FileExists(Application.InitrdFileName) then begin
  fMachineConfiguration.LoadINITRDFromFile(Application.InitrdFileName);
 end else if pvApplication.Assets.ExistAsset('riscv/'+Application.InitrdFileName) then begin
  Stream:=pvApplication.Assets.GetAssetStream('riscv/'+Application.InitrdFileName);
  if assigned(Stream) then begin
   try
    fMachineConfiguration.LoadINITRDFromStream(Stream);
   finally
    FreeAndNil(Stream);
   end;
  end;
 end;

 fMachine:=TPasRISCV.Create(fMachineConfiguration);

{$if (defined(fpc) and defined(unix)) or defined(Windows)}
 fFileSystem:=TPasRISCV9PFileSystemNative.Create(IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(pvApplication.Assets.BasePath)+'riscv')+'extern');
{$else}
 fFileSystem:=nil;
{$ifend}
 fMachine.VirtIO9PDevice.FileSystem:=fFileSystem;

{$if defined(fpc) and defined(unix)}
 fEthernetDevice:=TPasRISCVEthernetDeviceTUN.Create;
 TPasRISCVEthernetDeviceTUN(fEthernetDevice).Open('tap0');
{$else}
 fEthernetDevice:=nil;
{$ifend}
 fMachine.VirtIONetDevice.EthernetDevice:=fEthernetDevice;

 pvApplication.Audio.Lock;
 try
  pvApplication.Audio.OnFillBuffer:=fMachine.VirtIOSoundDevice.OutputAudioFillBufferCallback;
 finally
  pvApplication.Audio.Unlock;
 end;

 fMachine.FrameBufferDevice.AutomaticRefresh:=true;
 fMachine.FrameBufferDevice.Active:=true;

 fMachine.OnReboot:=OnReboot;
 fMachine.OnNewFrame:=OnNewFrame;

 inherited Create(false);
end;

destructor TMachineInstance.Destroy;
begin
 Shutdown;
 pvApplication.Audio.Lock;
 try
  pvApplication.Audio.OnFillBuffer:=nil;
 finally
  pvApplication.Audio.Unlock;
 end;
 fMachine.VirtIO9PDevice.FileSystem:=nil;
 begin
  if assigned(fEthernetDevice) then begin
   fEthernetDevice.Shutdown;
  end;
  fMachine.VirtIONetDevice.EthernetDevice:=nil;
 end;
 FreeAndNil(fMachine);
 FreeAndNil(fMachineConfiguration);
 FreeAndNil(fFileSystem);
 FreeAndNil(fEthernetDevice);
 fXCacheIntegers:=nil;
 inherited Destroy;
end;

procedure TMachineInstance.Shutdown;
begin
 if not Finished then begin
  Terminate;
  if assigned(fMachine) then begin
   fMachine.PowerOff;
   fMachine.WakeUp;
  end;
  WaitFor;
 end;
end;

procedure TMachineInstance.Execute;
{$ifdef unix}
var p:sched_param;
{$endif}
begin
 NameThreadForDebugging('TMachineInstance');
 Priority:=TThreadPriority.tpHighest;
{$ifdef unix}
 p.sched_priority:=40;
 pthread_setschedparam(Handle,0,@p);
{$endif}
 SetExceptionMask([exInvalidOp,exDenormalized,exZeroDivide,exOverflow,exUnderflow,exPrecision]);
 Boot;
 fMachine.Run;
//while not Terminated do;
end;

procedure TMachineInstance.ResetFrameBuffer;
begin
 fFrameBufferReadIndex:=0;
 fFrameBufferWriteIndex:=0;
end;

procedure TMachineInstance.Boot;
var Stream:TStream;
begin

 fMachine.Reset;

 if FileExists(Application.VirtIOBlockImageFileName) then begin
  Stream:=TFileStream.Create(Application.VirtIOBlockImageFileName,fmOpenReadWrite);
  if assigned(Stream) then begin
   try
    fMachine.VirtIOBlockDevice.AttachStream(Stream);
   except
    FreeAndNil(Stream);
   end;
  end;
 end else if FileExists(IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(pvApplication.Assets.BasePath)+'riscv')+Application.VirtIOBlockImageFileName) then begin
  Stream:=TFileStream.Create(IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(pvApplication.Assets.BasePath)+'riscv')+Application.VirtIOBlockImageFileName,fmOpenReadWrite);
  if assigned(Stream) then begin
   try
    fMachine.VirtIOBlockDevice.AttachStream(Stream);
   except
    FreeAndNil(Stream);
   end;
  end;
 end else if pvApplication.Assets.ExistAsset('riscv/'+Application.VirtIOBlockImageFileName) then begin
  Stream:=pvApplication.Assets.GetAssetStream('riscv/'+Application.VirtIOBlockImageFileName);
  if assigned(Stream) then begin
   try
    fMachine.VirtIOBlockDevice.LoadFromStream(Stream);
   finally
    FreeAndNil(Stream);
   end;
  end;
 end;

 if FileExists(Application.NVMeImageFileName) then begin
  Stream:=TFileStream.Create(Application.NVMeImageFileName,fmOpenReadWrite);
  if assigned(Stream) then begin
   try
    fMachine.NVMeDevice.AttachStream(Stream);
   except
    FreeAndNil(Stream);
   end;
  end;
 end else if FileExists(IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(pvApplication.Assets.BasePath)+'riscv')+Application.NVMeImageFileName) then begin
  Stream:=TFileStream.Create(IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(pvApplication.Assets.BasePath)+'riscv')+Application.NVMeImageFileName,fmOpenReadWrite);
  if assigned(Stream) then begin
   try
    fMachine.NVMeDevice.AttachStream(Stream);
   except
    FreeAndNil(Stream);
   end;
  end;
 end else if pvApplication.Assets.ExistAsset('riscv/'+Application.NVMeImageFileName) then begin
  Stream:=pvApplication.Assets.GetAssetStream('riscv/'+Application.NVMeImageFileName);
  if assigned(Stream) then begin
   try
    fMachine.NVMeDevice.LoadFromStream(Stream);
   finally
    FreeAndNil(Stream);
   end;
  end;
 end;

end;

procedure TMachineInstance.OnReboot;
begin
 Boot;
end;

procedure TMachineInstance.OnNewFrame;
var LocalReadIndex,LocalWriteIndex:TpvInt32;
    FrameBufferItem:PFrameBufferItem;
begin
 fMachine.FrameBufferDevice.Lock.AcquireRead;
 try
{$if not (defined(CPU386) or defined(CPUx86_64))}
  TPasMPMemoryBarrier.ReadWrite;
{$ifend}
  LocalReadIndex:=fFrameBufferReadIndex;
{$if defined(CPU386) or defined(CPUx86_64)}
  TPasMPMemoryBarrier.ReadDependency;
{$else}
  TPasMPMemoryBarrier.Read;
{$ifend}
  LocalWriteIndex:=(fFrameBufferWriteIndex+1) and 3;
  if LocalWriteIndex<>LocalReadIndex then begin
   FrameBufferItem:=@fFrameBufferItems[fFrameBufferWriteIndex];
   FrameBufferItem^.Width:=fMachine.FrameBufferDevice.Width;
   FrameBufferItem^.Height:=fMachine.FrameBufferDevice.Height;
   FrameBufferItem^.Active:=fMachine.FrameBufferDevice.Active;
   if fMachine.FrameBufferDevice.Active then begin
    Move(fMachine.FrameBufferDevice.Data[0],FrameBufferItem^.Data[0],FrameBufferItem^.Width*FrameBufferItem^.Height*SizeOf(TpvUInt32));
   end else begin
    FillChar(FrameBufferItem^.Data[0],FrameBufferItem^.Width*FrameBufferItem^.Height*SizeOf(TpvUInt32),#0);
   end;
{$ifdef CPU386}
   asm
    mfence
   end;
{$else}
   TPasMPMemoryBarrier.ReadWrite;
{$endif}
   fFrameBufferWriteIndex:=LocalWriteIndex;
  end;
 finally
  fMachine.FrameBufferDevice.Lock.ReleaseRead;
 end;
end;

function TMachineInstance.TransferFrame(const aFrameBuffer:Pointer;out aActive:Boolean):boolean;
var LocalReadIndex,LocalWriteIndex,Count,Index:TpvInt32;
    FrameBufferItem:PFrameBufferItem;
    SrcPixel,DstPixel:PPasRISCVUInt32;
begin
{$if not (defined(CPU386) or defined(CPUx86_64))}
 TPasMPMemoryBarrier.ReadWrite;
{$ifend}
 LocalReadIndex:=fFrameBufferReadIndex;
{$if defined(CPU386) or defined(CPUx86_64)}
 TPasMPMemoryBarrier.ReadDependency;
{$else}
 TPasMPMemoryBarrier.Read;
{$ifend}
 LocalWriteIndex:=fFrameBufferWriteIndex;
 if LocalReadIndex<>LocalWriteIndex then begin
  if LocalWriteIndex>=LocalReadIndex then begin
   Count:=LocalWriteIndex-LocalReadIndex;
  end else begin
   Count:=(Length(fFrameBufferItems)-LocalReadIndex)+LocalWriteIndex;
  end;
  if Count>0 then begin
   if Count>1 then begin
    LocalReadIndex:=(LocalReadIndex+(Count-1)) and 3;
{$ifdef CPU386}
    asm
     mfence
    end;
{$else}
    TPasMPMemoryBarrier.ReadWrite;
{$endif}
    fFrameBufferReadIndex:=LocalReadIndex;
   end;
   FrameBufferItem:=@fFrameBufferItems[LocalReadIndex];
   aActive:=FrameBufferItem^.Active;
   if (FrameBufferItem^.Width=ScreenWidth) and (FrameBufferItem^.Height=ScreenHeight) then begin
    SrcPixel:=Pointer(@FrameBufferItem^.Data[0]);
    DstPixel:=Pointer(aFrameBuffer);
    for Index:=1 to FrameBufferItem^.Width*FrameBufferItem^.Height do begin
     DstPixel^:=SrcPixel^ or TPasRISCVUInt32($ff000000);
     inc(SrcPixel);
     inc(DstPixel);
    end;
   end else begin
    ResizeRGB32(@FrameBufferItem^.Data[0],FrameBufferItem^.Width,FrameBufferItem^.Height,
                aFrameBuffer,ScreenWidth,ScreenHeight,
                fXCacheIntegers);
   end;
{$ifdef CPU386}
   asm
    mfence
   end;
{$else}
   TPasMPMemoryBarrier.ReadWrite;
{$endif}
   fFrameBufferReadIndex:=(LocalReadIndex+1) and 3;
   result:=true;
  end else begin
   result:=false;
  end;
 end else begin
  result:=false;
 end;
end;

{ TScreenEmulator }

constructor TScreenEmulator.Create;
begin
 inherited Create;
 fSelectedIndex:=-1;
 fReady:=false;
 fTerminalMode:=true;
 fLastTerminalMode:=not fTerminalMode;
 fMouseButtons:=0;
 fTime:=0.48;
 fTerm:=TPasTerm.Create(80,25);
 fTerm.OnDrawBackground:=DrawBackground;
 fTerm.OnDrawCodePoint:=DrawCodePoint;
 fTerm.OnDrawCursor:=DrawCursor;
 DrawBackground(fTerm);
//fTerm.Write(#27'%GHallo äöüßÖÄÜ');
 fTerminalFrameBufferSnapshot:=TPasTerm.TFrameBufferSnapshot.Create(fTerm);
 fTerminalFrameBufferSnapshot.Update;
end;

destructor TScreenEmulator.Destroy;
begin
 FreeAndNil(fTerminalFrameBufferSnapshot);
 FreeAndNil(fTerm);
 inherited Destroy;
end;

procedure TScreenEmulator.DrawBackground(const aSender:TPasTerm);
var Index:TpvSizeInt;
begin
 for Index:=0 to (TMachineInstance.ScreenWidth*TMachineInstance.ScreenHeight)-1 do begin
  fTerminalFrameBuffer[Index]:=TpvUInt32($ff000000);
 end;
end;

procedure TScreenEmulator.DrawCodePoint(const aSender:TPasTerm;const aCodePoint:TPasTerm.TFrameBufferCodePoint;const aColumn,aRow:TPasTermSizeInt);
var Page,CharIndex,Color:TpvUInt32;
    Font8Char:PVGAFont8Char;
    Font16Char:PVGAFont16Char;
    BaseX,BaseY,x,y,ox,oy:TpvInt32;
begin
 if aCodePoint.CodePoint<=$1ffff then begin
  Page:=VGAFontMapPageMap[aCodePoint.CodePoint shr 8];
  if Page<>0 then begin
   CharIndex:=VGAFontMapPages[Page-1,aCodePoint.CodePoint and $ff];
   if (CharIndex and TpvUInt32($03000000))<>0 then begin
    if (CharIndex and TpvUInt32($01000000))<>0 then begin
     Font8Char:=@VGAFont8Chars[CharIndex and TpvUInt32($00ffffff)];
     BaseX:=aColumn*8;
     BaseY:=aRow*16;
     if (BaseX>=0) and ((BaseX+8)<=TMachineInstance.ScreenWidth) and (BaseY>=0) and ((BaseY+16)<=TMachineInstance.ScreenHeight) then begin
      for y:=0 to 15 do begin
       oy:=BaseY+y;
       for x:=0 to 7 do begin
        ox:=BaseX+x;
        if (Font8Char^[y] and (1 shl x))<>0 then begin
         Color:=aCodePoint.ForegroundColor;
        end else begin
         Color:=aCodePoint.BackgroundColor;
        end;
        fTerminalFrameBuffer[(oy*TMachineInstance.ScreenWidth)+ox]:=Color or $ff000000;
       end;
      end;
     end;
    end else if (CharIndex and TpvUInt32($02000000))<>0 then begin
     Font16Char:=@VGAFont16Chars[CharIndex and TpvUInt32($00ffffff)];
     BaseX:=aColumn*8;
     BaseY:=aRow*16;
     if (BaseX>=0) and ((BaseX+16)<=TMachineInstance.ScreenWidth) and (BaseY>=0) and ((BaseY+16)<=TMachineInstance.ScreenHeight) then begin
      for y:=0 to 15 do begin
       oy:=BaseY+y;
       for x:=0 to 15 do begin
        ox:=BaseX+x;
        if (Font16Char^[y] and (1 shl x))<>0 then begin
         Color:=aCodePoint.ForegroundColor;
        end else begin
         Color:=aCodePoint.BackgroundColor;
        end;
        fTerminalFrameBuffer[(oy*TMachineInstance.ScreenWidth)+ox]:=Color or $ff000000;
       end;
      end;
     end;
    end;
   end else begin
    BaseX:=aColumn*8;
    BaseY:=aRow*16;
    if (BaseX>=0) and ((BaseX+8)<=TMachineInstance.ScreenWidth) and (BaseY>=0) and ((BaseY+16)<=TMachineInstance.ScreenHeight) then begin
     Color:=aCodePoint.BackgroundColor;
     for y:=0 to 15 do begin
      oy:=BaseY+y;
      for x:=0 to 7 do begin
       ox:=BaseX+x;
       fTerminalFrameBuffer[(oy*TMachineInstance.ScreenWidth)+ox]:=Color or $ff000000;
      end;
     end;
    end;
   end;
  end;
 end;
end;

procedure TScreenEmulator.DrawCursor(const aSender:TPasTerm;const aColumn,aRow:TPasTermSizeInt);
var CodePoint:TPasTerm.TFrameBufferCodePoint;
    t:TPasTermUInt32;
begin
 CodePoint:=aSender.GetCodePoint(aColumn,aRow,true);
 t:=CodePoint.ForegroundColor;
 CodePoint.ForegroundColor:=CodePoint.BackgroundColor;
 CodePoint.BackgroundColor:=t;
 DrawCodePoint(aSender,CodePoint,aColumn,aRow);
end;

procedure TScreenEmulator.Show;
const CacheVersionGUID:TGUID='{8591FC7C-8BC8-4724-BA68-EDF89292CF32}';
var Stream:TStream;
    Index,x,y:TpvInt32;
    RawSprite:pointer;
    TrueTypeFont:TpvTrueTypeFont;
    RecreateCacheFiles:boolean;
    CacheStoragePath,CacheStorageFile:string;
    FileStream:TFileStream;
    CacheStorageCacheVersionGUID:TGUID;
begin
 inherited Show;

 fVulkanGraphicsCommandPool:=TpvVulkanCommandPool.Create(pvApplication.VulkanDevice,
                                                         pvApplication.VulkanDevice.GraphicsQueueFamilyIndex,
                                                         TVkCommandPoolCreateFlags(VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT));

 fVulkanGraphicsCommandBuffer:=TpvVulkanCommandBuffer.Create(fVulkanGraphicsCommandPool,VK_COMMAND_BUFFER_LEVEL_PRIMARY);

 fVulkanGraphicsCommandBufferFence:=TpvVulkanFence.Create(pvApplication.VulkanDevice);

 fVulkanTransferCommandPool:=TpvVulkanCommandPool.Create(pvApplication.VulkanDevice,
                                                         pvApplication.VulkanDevice.TransferQueueFamilyIndex,
                                                         TVkCommandPoolCreateFlags(VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT));

 fVulkanTransferCommandBuffer:=TpvVulkanCommandBuffer.Create(fVulkanTransferCommandPool,VK_COMMAND_BUFFER_LEVEL_PRIMARY);

 fVulkanTransferCommandBufferFence:=TpvVulkanFence.Create(pvApplication.VulkanDevice);

 fFrameBufferTextureSampler:=TpvVulkanSampler.Create(pvApplication.VulkanDevice,
                                                     VK_FILTER_LINEAR,
                                                     VK_FILTER_LINEAR,
                                                     VK_SAMPLER_MIPMAP_MODE_LINEAR,
                                                     VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE,
                                                     VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE,
                                                     VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE,
                                                     0.0,
                                                     false,
                                                     0.0,
                                                     false,
                                                     VK_COMPARE_OP_ALWAYS,
                                                     0.0,
                                                     1000.0,
                                                     VK_BORDER_COLOR_INT_OPAQUE_BLACK,
                                                     false,
                                                     VK_SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE);

 for Index:=0 to pvApplication.CountInFlightFrames-1 do begin
  fFrameBufferTextures[Index]:=TpvVulkanTexture.CreateFromMemory(pvApplication.VulkanDevice,
                                                                 pvApplication.VulkanDevice.GraphicsQueue,
                                                                 fVulkanGraphicsCommandBuffer,
                                                                 fVulkanGraphicsCommandBufferFence,
                                                                 pvApplication.VulkanDevice.TransferQueue,
                                                                 fVulkanTransferCommandBuffer,
                                                                 fVulkanTransferCommandBufferFence,
                                                                 VK_FORMAT_R8G8B8A8_SRGB,
                                                                 VK_SAMPLE_COUNT_1_BIT,
                                                                 TMachineInstance.ScreenWidth,
                                                                 TMachineInstance.ScreenHeight,
                                                                 0,
                                                                 0,
                                                                 1,
                                                                 -1,
                                                                 [TpvVulkanTextureUsageFlag.General,TpvVulkanTextureUsageFlag.Sampled,TpvVulkanTextureUsageFlag.TransferDst,TpvVulkanTextureUsageFlag.TransferSrc],
                                                                 @fGraphicsFrameBuffer[0],
                                                                 SizeOf(TMachineInstance.TFrameBuffer),
                                                                 false,
                                                                 false,
                                                                 0,
                                                                 true,
                                                                 false,
                                                                 true,
                                                                 0);
  fFrameBufferTextures[Index].Sampler:=fFrameBufferTextureSampler;
  fFrameBufferTextures[Index].UpdateDescriptorImageInfo;
  fFrameBufferGenerations[Index]:=High(TpvUInt64);
  fFrameBufferTextureGenerations[Index]:=High(TpvUInt64);
 end;

 fFrameBufferGeneration:=0;

 fVulkanCommandPool:=TpvVulkanCommandPool.Create(pvApplication.VulkanDevice,
                                                 pvApplication.VulkanDevice.GraphicsQueueFamilyIndex,
                                                 TVkCommandPoolCreateFlags(VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT));
 for Index:=0 to MaxInFlightFrames-1 do begin
  fVulkanRenderCommandBuffers[Index]:=TpvVulkanCommandBuffer.Create(fVulkanCommandPool,VK_COMMAND_BUFFER_LEVEL_PRIMARY);
  fVulkanRenderSemaphores[Index]:=TpvVulkanSemaphore.Create(pvApplication.VulkanDevice);
 end;

 fVulkanRenderPass:=nil;

 fVulkanCanvas:=TpvCanvas.Create(pvApplication.VulkanDevice,
                                 pvApplication.VulkanPipelineCache,
                                 MaxInFlightFrames);

{fVulkanSpriteAtlas:=TpvSpriteAtlas.Create(pvApplication.VulkanDevice,true);
 fVulkanSpriteAtlas.UseConvexHullTrimming:=false;

 fVulkanFontSpriteAtlas:=TpvSpriteAtlas.Create(pvApplication.VulkanDevice,false);
 fVulkanFontSpriteAtlas.MipMaps:=false;
 fVulkanFontSpriteAtlas.UseConvexHullTrimming:=false;

 RecreateCacheFiles:=true;

 if pvApplication.Files.IsCacheStorageAvailable then begin

  CacheStoragePath:=IncludeTrailingPathDelimiter(pvApplication.Files.GetCacheStoragePath);

  CacheStorageFile:=CacheStoragePath+'terminal_cache_version.dat';

  if FileExists(CacheStorageFile) and
     FileExists(CacheStoragePath+'terminal_font.dat') and
     FileExists(CacheStoragePath+'terminal_spriteatlas.zip') then begin

   FileStream:=TFileStream.Create(CacheStorageFile,fmOpenRead or fmShareDenyWrite);
   try
    FileStream.Read(CacheStorageCacheVersionGUID,SizeOf(TGUID));
   finally
    FileStream.Free;
   end;

   if CompareMem(@CacheStorageCacheVersionGUID,@CacheVersionGUID,SizeOf(TGUID)) then begin

    //RecreateCacheFiles:=false;

   end;

  end;

 end else begin

  CacheStoragePath:='';

 end;

 if RecreateCacheFiles then begin

  //Stream:=pvApplication.Assets.GetAssetStream('fonts/linbiolinum_r.otf');
  //Stream:=pvApplication.Assets.GetAssetStream('fonts/notosans.ttf');
  Stream:=pvApplication.Assets.GetAssetStream('fonts/vga.ttf');
  try
   TrueTypeFont:=TpvTrueTypeFont.Create(Stream,72);
   try
    TrueTypeFont.Size:=-64;
    TrueTypeFont.Hinting:=false;
    fVulkanFont:=TpvFont.CreateFromTrueTypeFont(fVulkanFontSpriteAtlas,
                                                TrueTypeFont,
                                                [TpvFontCodePointRange.Create(0,65535)],
                                                true,
                                                2,
                                                1);
    if length(CacheStoragePath)>0 then begin
     fVulkanFont.SaveToFile(CacheStoragePath+'terminal_font.dat');
    end;
   finally
    TrueTypeFont.Free;
   end;
  finally
   Stream.Free;
  end;

  if length(CacheStoragePath)>0 then begin

   fVulkanFontSpriteAtlas.SaveToFile(CacheStoragePath+'terminal_font_spriteatlas.zip',true);

   fVulkanSpriteAtlas.SaveToFile(CacheStoragePath+'terminal_spriteatlas.zip',true);

   FileStream:=TFileStream.Create(CacheStoragePath+'terminal_cache_version.dat',fmCreate);
   try
    FileStream.Write(CacheVersionGUID,SizeOf(TGUID));
   finally
    FileStream.Free;
   end;

  end;

 end else begin

  fVulkanFontSpriteAtlas.LoadFromFile(CacheStoragePath+'terminal_font_spriteatlas.zip');

  fVulkanFont:=TpvFont.CreateFromFile(fVulkanFontSpriteAtlas,CacheStoragePath+'terminal_font.dat');

  fVulkanSpriteAtlas.LoadFromFile(CacheStoragePath+'terminal_spriteatlas.zip');

 end;

 fVulkanFontSpriteAtlas.Upload(pvApplication.VulkanDevice.GraphicsQueue,
                               fVulkanGraphicsCommandBuffer,
                               fVulkanGraphicsCommandBufferFence,
                               pvApplication.VulkanDevice.TransferQueue,
                               fVulkanTransferCommandBuffer,
                               fVulkanTransferCommandBufferFence);

 fVulkanSpriteAtlas.Upload(pvApplication.VulkanDevice.GraphicsQueue,
                           fVulkanGraphicsCommandBuffer,
                           fVulkanGraphicsCommandBufferFence,
                           pvApplication.VulkanDevice.TransferQueue,
                           fVulkanTransferCommandBuffer,
                           fVulkanTransferCommandBufferFence);    //}

 fMachineInstance:=TMachineInstance.Create;

 fContentGeneration:=0;
 for Index:=0 to MaxInFlightFrames-1 do begin
  fRenderGeneration[Index]:=High(TpvUInt64);
 end;
 fTextureGeneration:=High(TpvUInt64);

end;

procedure TScreenEmulator.Hide;
var Index:TpvInt32;
begin
 fMachineInstance.Shutdown;
 FreeAndNil(fMachineInstance);
{FreeAndNil(fVulkanFont);
 FreeAndNil(fVulkanFontSpriteAtlas);
 FreeAndNil(fVulkanSpriteAtlas);}
 FreeAndNil(fVulkanCanvas);
 FreeAndNil(fVulkanRenderPass);
 for Index:=0 to MaxInFlightFrames-1 do begin
  FreeAndNil(fVulkanRenderCommandBuffers[Index]);
  FreeAndNil(fVulkanRenderSemaphores[Index]);
 end;
 FreeAndNil(fVulkanCommandPool);
 for Index:=0 to pvApplication.CountInFlightFrames-1 do begin
  FreeAndNil(fFrameBufferTextures[Index]);
 end;
 FreeAndNil(fFrameBufferTextureSampler);
 FreeAndNil(fVulkanTransferCommandBufferFence);
 FreeAndNil(fVulkanTransferCommandBuffer);
 FreeAndNil(fVulkanTransferCommandPool);
 FreeAndNil(fVulkanGraphicsCommandBufferFence);
 FreeAndNil(fVulkanGraphicsCommandBuffer);
 FreeAndNil(fVulkanGraphicsCommandPool);
 inherited Hide;
end;

procedure TScreenEmulator.Resume;
begin
 inherited Resume;
end;

procedure TScreenEmulator.Pause;
begin
 inherited Pause;
end;

procedure TScreenEmulator.Resize(const aWidth,aHeight:TpvInt32);
begin
 inherited Resize(aWidth,aHeight);
end;

procedure TScreenEmulator.AfterCreateSwapChain;
var Index:TpvInt32;
begin
 inherited AfterCreateSwapChain;

 FreeAndNil(fVulkanRenderPass);

 fVulkanRenderPass:=TpvVulkanRenderPass.Create(pvApplication.VulkanDevice);

 fVulkanRenderPass.AddSubpassDescription(0,
                                         VK_PIPELINE_BIND_POINT_GRAPHICS,
                                         [],
                                         [fVulkanRenderPass.AddAttachmentReference(fVulkanRenderPass.AddAttachmentDescription(0,
                                                                                                                              pvApplication.VulkanSwapChain.ImageFormat,
                                                                                                                              VK_SAMPLE_COUNT_1_BIT,
                                                                                                                              VK_ATTACHMENT_LOAD_OP_CLEAR,
                                                                                                                              VK_ATTACHMENT_STORE_OP_STORE,
                                                                                                                              VK_ATTACHMENT_LOAD_OP_DONT_CARE,
                                                                                                                              VK_ATTACHMENT_STORE_OP_DONT_CARE,
                                                                                                                              VK_IMAGE_LAYOUT_UNDEFINED, //VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL, //VK_IMAGE_LAYOUT_UNDEFINED, // VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
                                                                                                                              VK_IMAGE_LAYOUT_PRESENT_SRC_KHR //VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL //VK_IMAGE_LAYOUT_PRESENT_SRC_KHR  // VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
                                                                                                                             ),
                                                                             VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
                                                                            )],
                                         [],
                                         fVulkanRenderPass.AddAttachmentReference(fVulkanRenderPass.AddAttachmentDescription(0,
                                                                                                                             pvApplication.VulkanDepthImageFormat,
                                                                                                                             VK_SAMPLE_COUNT_1_BIT,
                                                                                                                             VK_ATTACHMENT_LOAD_OP_CLEAR,
                                                                                                                             VK_ATTACHMENT_STORE_OP_DONT_CARE,
                                                                                                                             VK_ATTACHMENT_LOAD_OP_DONT_CARE,
                                                                                                                             VK_ATTACHMENT_STORE_OP_DONT_CARE,
                                                                                                                             VK_IMAGE_LAYOUT_UNDEFINED, //VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL, // VK_IMAGE_LAYOUT_UNDEFINED, // VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL,
                                                                                                                             VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL
                                                                                                                            ),
                                                                                  VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL
                                                                                 ),
                                         []);
  fVulkanRenderPass.AddSubpassDependency(VK_SUBPASS_EXTERNAL,
                                         0,
                                         TVkPipelineStageFlags(VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT),
                                         TVkPipelineStageFlags(VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT),
                                         TVkAccessFlags(VK_ACCESS_MEMORY_READ_BIT),
                                         TVkAccessFlags(VK_ACCESS_COLOR_ATTACHMENT_READ_BIT) or TVkAccessFlags(VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT) or TVkAccessFlags(VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT) or TVkAccessFlags(VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT),
                                         TVkDependencyFlags(VK_DEPENDENCY_BY_REGION_BIT));
  fVulkanRenderPass.AddSubpassDependency(0,
                                         VK_SUBPASS_EXTERNAL,
                                         TVkPipelineStageFlags(VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT),
                                         TVkPipelineStageFlags(VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT),
                                         TVkAccessFlags(VK_ACCESS_COLOR_ATTACHMENT_READ_BIT) or TVkAccessFlags(VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT) or TVkAccessFlags(VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT) or TVkAccessFlags(VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT),
                                         TVkAccessFlags(VK_ACCESS_MEMORY_READ_BIT),
                                         TVkDependencyFlags(VK_DEPENDENCY_BY_REGION_BIT));
  fVulkanRenderPass.Initialize;

  fVulkanRenderPass.ClearValues[0].color.float32[0]:=0.0;
  fVulkanRenderPass.ClearValues[0].color.float32[1]:=0.0;
  fVulkanRenderPass.ClearValues[0].color.float32[2]:=0.0;
  fVulkanRenderPass.ClearValues[0].color.float32[3]:=1.0;
{ fVulkanRenderPass.AddSubpassDependency(VK_SUBPASS_EXTERNAL,
                                        0,
                                        TVkPipelineStageFlags(VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT),
                                        TVkPipelineStageFlags(VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT),
                                        TVkAccessFlags(VK_ACCESS_MEMORY_READ_BIT),
                                        TVkAccessFlags(VK_ACCESS_COLOR_ATTACHMENT_READ_BIT) or TVkAccessFlags(VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT),
                                        TVkDependencyFlags(VK_DEPENDENCY_BY_REGION_BIT));
 fVulkanRenderPass.AddSubpassDependency(0,
                                        VK_SUBPASS_EXTERNAL,
                                        TVkPipelineStageFlags(VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT),
                                        TVkPipelineStageFlags(VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT),
                                        TVkAccessFlags(VK_ACCESS_COLOR_ATTACHMENT_READ_BIT) or TVkAccessFlags(VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT),
                                        TVkAccessFlags(VK_ACCESS_MEMORY_READ_BIT),
                                        TVkDependencyFlags(VK_DEPENDENCY_BY_REGION_BIT));
 fVulkanRenderPass.Initialize;

 fVulkanRenderPass.ClearValues[0].color.float32[0]:=0.0;
 fVulkanRenderPass.ClearValues[0].color.float32[1]:=0.0;
 fVulkanRenderPass.ClearValues[0].color.float32[2]:=0.0;
 fVulkanRenderPass.ClearValues[0].color.float32[3]:=1.0;  }

 fVulkanCanvas.VulkanRenderPass:=fVulkanRenderPass;
 fVulkanCanvas.CountBuffers:=pvApplication.CountInFlightFrames;
{if pvApplication.Width<pvApplication.Height then begin
  fVulkanCanvas.Width:=(720*pvApplication.Width) div pvApplication.Height;
  fVulkanCanvas.Height:=720;
 end else begin
  fVulkanCanvas.Width:=1280;
  fVulkanCanvas.Height:=(1280*pvApplication.Height) div pvApplication.Width;
 end;}
{fVulkanCanvas.Width:=640;
 fVulkanCanvas.Height:=400;}
 fVulkanCanvas.Width:=pvApplication.Width;
 fVulkanCanvas.Height:=pvApplication.Height;
 fVulkanCanvas.Viewport.x:=0;
 fVulkanCanvas.Viewport.y:=0;
 fVulkanCanvas.Viewport.width:=pvApplication.Width;
 fVulkanCanvas.Viewport.height:=pvApplication.Height;

 for Index:=0 to length(fVulkanRenderCommandBuffers)-1 do begin
  FreeAndNil(fVulkanRenderCommandBuffers[Index]);
  fVulkanRenderCommandBuffers[Index]:=TpvVulkanCommandBuffer.Create(fVulkanCommandPool,VK_COMMAND_BUFFER_LEVEL_PRIMARY);
 end;

end;

procedure TScreenEmulator.BeforeDestroySwapChain;
begin
 fVulkanCanvas.VulkanRenderPass:=nil;
 FreeAndNil(fVulkanRenderPass);
 inherited BeforeDestroySwapChain;
end;

function TScreenEmulator.KeyEvent(const aKeyEvent:TpvApplicationInputKeyEvent):boolean;
 procedure Send(const u:TpvRawByteString);
 var c:AnsiChar;
 begin
  if length(u)>0 then begin
   for c in u do begin
    fMachineInstance.fMachine.UARTDevice.InputQueue.Enqueue(c);
    fMachineInstance.fMachine.UARTDevice.Notify;
   end;
   fMachineInstance.fMachine.WakeUp;
  end;
 end;
var c:AnsiChar;
    u,m:TpvRawByteString;
    v:TPasTermUInt8;
begin
 result:=false;
 if (aKeyEvent.KeyModifiers*[TpvApplicationInputKeyModifier.ALT,TpvApplicationInputKeyModifier.CTRL,TpvApplicationInputKeyModifier.SHIFT])=[TpvApplicationInputKeyModifier.CTRL] then begin
  case aKeyEvent.KeyCode of
   KEYCODE_F12:begin
    case aKeyEvent.KeyEventType of
     TpvApplicationInputKeyEventType.Down:begin
      fTerminalMode:=not fTerminalMode;
     end;
    end;
    result:=true;
    exit;
   end;
  end;
 end;
 if fTerminalMode then begin
  case aKeyEvent.KeyEventType of
   TpvApplicationInputKeyEventType.Typed:begin
    v:=0;
    if TpvApplicationInputKeyModifier.SHIFT in aKeyEvent.KeyModifiers then begin
     v:=v or 1;
    end;
    if TpvApplicationInputKeyModifier.ALT in aKeyEvent.KeyModifiers then begin
     v:=v or 2;
    end;
    if TpvApplicationInputKeyModifier.CTRL in aKeyEvent.KeyModifiers then begin
     v:=v or 4;
    end;
    if TpvApplicationInputKeyModifier.META in aKeyEvent.KeyModifiers then begin
     v:=v or 8;
    end;
    if v<>0 then begin
     m:=IntToStr(v+1);
    end else begin
     m:='';
    end;
    case aKeyEvent.KeyCode of
     KEYCODE_UP:begin
      if length(m)>0 then begin
       if m='3' then begin
        m:='5';
       end;
       Send(#$1b'[1;'+m+'A');
      end else if TpvApplicationInputKeyModifier.SHIFT in aKeyEvent.KeyModifiers then begin
       Send(#$1b'OA');
      end else if TpvApplicationInputKeyModifier.CTRL in aKeyEvent.KeyModifiers then begin
       Send(#$1b'OA');
      end else begin
       Send(#$1b'[A');
      end;
     end;
     KEYCODE_DOWN:begin
      if length(m)>0 then begin
       if m='3' then begin
        m:='5';
       end;
       Send(#$1b'[1;'+m+'B');
      end else if TpvApplicationInputKeyModifier.SHIFT in aKeyEvent.KeyModifiers then begin
       Send(#$1b'OB');
      end else if TpvApplicationInputKeyModifier.CTRL in aKeyEvent.KeyModifiers then begin
       Send(#$1b'OB');
      end else begin
       Send(#$1b'[B');
      end;
     end;
     KEYCODE_RIGHT:begin
      if length(m)>0 then begin
       if m='3' then begin
        m:='5';
       end;
       Send(#$1b'[1;'+m+'C');
      end else if TpvApplicationInputKeyModifier.SHIFT in aKeyEvent.KeyModifiers then begin
       Send(#$1b'OC');
      end else if TpvApplicationInputKeyModifier.CTRL in aKeyEvent.KeyModifiers then begin
       Send(#$1b'OC');
      end else begin
       Send(#$1b'[C');
      end;
     end;
     KEYCODE_LEFT:begin
      if length(m)>0 then begin
       if m='3' then begin
        m:='5';
       end;
       Send(#$1b'[1;'+m+'D');
      end else if TpvApplicationInputKeyModifier.SHIFT in aKeyEvent.KeyModifiers then begin
       Send(#$1b'OD');
      end else if TpvApplicationInputKeyModifier.CTRL in aKeyEvent.KeyModifiers then begin
       Send(#$1b'OD');
      end else begin
       Send(#$1b'[D');
      end;
     end;
     KEYCODE_HOME:begin
      if length(m)>0 then begin
 //    Send(#$1b'[1;'+m+'H');
       Send(#$1b'[1;'+m+'~');
      end else begin
 //    Send(#$1b'[H');
       Send(#$1b'[1~');
      end;
     end;
     KEYCODE_END:begin
      if length(m)>0 then begin
 //    Send(#$1b'[1;'+m+'F');
       Send(#$1b'[4;'+m+'~');
      end else begin
 //    Send(#$1b'[F');
       Send(#$1b'[4~');
      end;
     end;
     KEYCODE_PAGEDOWN:begin
      if length(m)>0 then begin
       Send(#$1b'[6;'+m+'~');
      end else begin
       Send(#$1b'[6~');
      end;
     end;
     KEYCODE_PAGEUP:begin
      if length(m)>0 then begin
       Send(#$1b'[5;'+m+'~');
      end else begin
       Send(#$1b'[5~');
      end;
     end;
     KEYCODE_F1:begin
      if length(m)>0 then begin
       Send(#$1b'[1;'+m+'P');
      end else begin
 //    Send(#$1b'[OP');
       Send(#$1b'[[A');
      end;
     end;
     KEYCODE_F2:begin
      if length(m)>0 then begin
       Send(#$1b'[1;'+m+'Q');
      end else begin
 //    Send(#$1b'[OQ');
       Send(#$1b'[[B');
      end;
     end;
     KEYCODE_F3:begin
      if length(m)>0 then begin
       Send(#$1b'[1;'+m+'R');
      end else begin
 //    Send(#$1b'[OR');
       Send(#$1b'[[C');
      end;
     end;
     KEYCODE_F4:begin
      if length(m)>0 then begin
       Send(#$1b'[1;'+m+'S');
      end else begin
 //    Send(#$1b'[OS');
       Send(#$1b'[[D');
      end;
     end;
     KEYCODE_F5:begin
      if length(m)>0 then begin
       Send(#$1b'[15;'+m+'~');
      end else begin
 //    Send(#$1b'[15~');
       Send(#$1b'[[E');
      end;
     end;
     KEYCODE_F6:begin
      if length(m)>0 then begin
       Send(#$1b'[17;'+m+'~');
      end else begin
       Send(#$1b'[17~');
      end;
     end;
     KEYCODE_F7:begin
      if length(m)>0 then begin
       Send(#$1b'[18;'+m+'~');
      end else begin
       Send(#$1b'[18~');
      end;
     end;
     KEYCODE_F8:begin
      if length(m)>0 then begin
       Send(#$1b'[19;'+m+'~');
      end else begin
       Send(#$1b'[19~');
      end;
     end;
     KEYCODE_F9:begin
      if length(m)>0 then begin
       Send(#$1b'[20;'+m+'~');
      end else begin
       Send(#$1b'[20~');
      end;
     end;
     KEYCODE_F10:begin
      if length(m)>0 then begin
       Send(#$1b'[21;'+m+'~');
      end else begin
       Send(#$1b'[21~');
      end;
     end;
     KEYCODE_F11:begin
      if length(m)>0 then begin
       Send(#$1b'[23;'+m+'~');
      end else begin
       Send(#$1b'[23~');
      end;
     end;
     KEYCODE_F12:begin
      if length(m)>0 then begin
       Send(#$1b'[24;'+m+'~');
      end else begin
       Send(#$1b'[24~');
      end;
     end;
     KEYCODE_TAB:begin
      if TpvApplicationInputKeyModifier.SHIFT in aKeyEvent.KeyModifiers then begin
       Send(#$1b'[Z');
      end else begin
       Send(#9);
      end;
     end;
     KEYCODE_BACKSPACE:begin
      if TpvApplicationInputKeyModifier.SHIFT in aKeyEvent.KeyModifiers then begin
       Send(#$7f);
      end else if TpvApplicationInputKeyModifier.CTRL in aKeyEvent.KeyModifiers then begin
       Send(#$8);
      end else begin
       Send(#$7f);
      end;
     end;
     KEYCODE_KP_ENTER,
     KEYCODE_RETURN:begin
      if TpvApplicationInputKeyModifier.ALT in aKeyEvent.KeyModifiers then begin
       Send(#$1b#10);
      end else begin
       Send(#10);
      end;
     end;
     KEYCODE_ESCAPE:begin
      if TpvApplicationInputKeyModifier.ALT in aKeyEvent.KeyModifiers then begin
       Send(#$1b);
      end else begin
       Send(#$1b#$1b);
      end;
     end;
     KEYCODE_DELETE:begin
      if length(m)>0 then begin
       Send(#$1b'[3;'+m+'~');
      end else begin
       Send(#$1b'[3~');
      end;
     end;
     KEYCODE_INSERT:begin
      if length(m)>0 then begin
       Send(#$1b'[2;'+m+'~');
      end else begin
       Send(#$1b'[2~');
      end;
     end;
     KEYCODE_A..KEYCODE_Z:begin
      if TpvApplicationInputKeyModifier.CTRL in aKeyEvent.KeyModifiers then begin
       Send(Chr((aKeyEvent.KeyCode-KEYCODE_A)+1));
      end;
     end;
    end;
   end;
   TpvApplicationInputKeyEventType.Unicode:begin
    if (TpvApplicationInputKeyModifier.CTRL in aKeyEvent.KeyModifiers) and
       (((aKeyEvent.KeyCode>=ord('a')) or (aKeyEvent.KeyCode<=ord('z'))) or
        ((aKeyEvent.KeyCode>=ord('A')) or (aKeyEvent.KeyCode<=ord('Z'))) or
        (aKeyEvent.KeyCode=ord('@'))) then begin
 //  Send(#$1b+LowerCase(Chr(aKeyEvent.KeyCode)));
    end else begin
     Send(PUCUUTF32CharToUTF8(aKeyEvent.KeyCode));
    end;
   end;
   TpvApplicationInputKeyEventType.Down:begin
    fMachineInstance.fMachine.RawKeyboardDevice.KeyDown(aKeyEvent.ScanCode);
   end;
   TpvApplicationInputKeyEventType.Up:begin
    fMachineInstance.fMachine.RawKeyboardDevice.KeyUp(aKeyEvent.ScanCode);
   end;
  end;
 end else begin
  if not fMachineInstance.fMachine.FrameBufferDevice.AutomaticRefresh then begin
   case aKeyEvent.KeyEventType of
    TpvApplicationInputKeyEventType.Down:begin
     fMachineInstance.fMachine.RawKeyboardDevice.KeyDown(aKeyEvent.ScanCode);
    end;
    TpvApplicationInputKeyEventType.Up:begin
     fMachineInstance.fMachine.RawKeyboardDevice.KeyUp(aKeyEvent.ScanCode);
    end;
    else begin
    end;
   end;
  end;
  case aKeyEvent.KeyEventType of
   TpvApplicationInputKeyEventType.Down,
   TpvApplicationInputKeyEventType.Up:begin
    if assigned(fMachineInstance.fMachine.I2CHIDKeyboardBusDevice) then begin
     fMachineInstance.fMachine.I2CHIDKeyboardBusDevice.HandleKeyboard(MapKeyCodeToHIDKeyCode(aKeyEvent.ScanCode),aKeyEvent.KeyEventType=TpvApplicationInputKeyEventType.Down);
    end;
    if assigned(fMachineInstance.fMachine.PS2KeyboardDevice) then begin
     fMachineInstance.fMachine.PS2KeyboardDevice.HandleKeyboard(MapKeyCodeToHIDKeyCode(aKeyEvent.ScanCode),aKeyEvent.KeyEventType=TpvApplicationInputKeyEventType.Down);
    end;
    if assigned(fMachineInstance.fMachine.VirtIOInputKeyboardDevice) then begin
     fMachineInstance.fMachine.VirtIOInputKeyboardDevice.HandleKeyboard(MapKeyCodeToEVDEVKeyCode(aKeyEvent.ScanCode),aKeyEvent.KeyEventType=TpvApplicationInputKeyEventType.Down);
    end;
   end;
   else begin
   end;
  end;
 end;
 result:=true;
end;

function TScreenEmulator.PointerEvent(const aPointerEvent:TpvApplicationInputPointerEvent):boolean;
var Index:TpvInt32;
    cy:TpvFloat;
begin
 result:=false;
 if fReady then begin
  if fTerminalMode then begin
   case aPointerEvent.PointerEventType of
    TpvApplicationInputPointerEventType.Down:begin
    end;
    TpvApplicationInputPointerEventType.Up:begin
    end;
    TpvApplicationInputPointerEventType.Motion:begin
    end;
    TpvApplicationInputPointerEventType.Drag:begin
    end;
   end;
  end else begin
   if assigned(fMachineInstance.fMachine.PS2MouseDevice) then begin
    if (aPointerEvent.RelativePosition.x<>0) or (aPointerEvent.RelativePosition.y<>0) then begin
     fMachineInstance.fMachine.PS2MouseDevice.RelativeMove(round(aPointerEvent.RelativePosition.x),round(aPointerEvent.RelativePosition.y));
    end;
    case aPointerEvent.PointerEventType of
     TpvApplicationInputPointerEventType.Down:begin
      case aPointerEvent.Button of
       TpvApplicationInputPointerButton.Left:begin
        fMachineInstance.fMachine.PS2MouseDevice.ButtonPress(TPasRISCV.TPS2MouseDevice.BTN_LEFT);
       end;
       TpvApplicationInputPointerButton.Right:begin
        fMachineInstance.fMachine.PS2MouseDevice.ButtonPress(TPasRISCV.TPS2MouseDevice.BTN_RIGHT);
       end;
       TpvApplicationInputPointerButton.Middle:begin
        fMachineInstance.fMachine.PS2MouseDevice.ButtonPress(TPasRISCV.TPS2MouseDevice.BTN_MIDDLE);
       end;
       else begin
       end;
      end;
     end;
     TpvApplicationInputPointerEventType.Up:begin
      case aPointerEvent.Button of
       TpvApplicationInputPointerButton.Left:begin
        fMachineInstance.fMachine.PS2MouseDevice.ButtonRelease(TPasRISCV.TPS2MouseDevice.BTN_LEFT);
       end;
       TpvApplicationInputPointerButton.Right:begin
        fMachineInstance.fMachine.PS2MouseDevice.ButtonRelease(TPasRISCV.TPS2MouseDevice.BTN_RIGHT);
       end;
       TpvApplicationInputPointerButton.Middle:begin
        fMachineInstance.fMachine.PS2MouseDevice.ButtonRelease(TPasRISCV.TPS2MouseDevice.BTN_MIDDLE);
       end;
       else begin
       end;
      end;
     end;
     TpvApplicationInputPointerEventType.Motion:begin
     end;
     else begin
     end;
    end;
   end;
   case aPointerEvent.PointerEventType of
    TpvApplicationInputPointerEventType.Down,
    TpvApplicationInputPointerEventType.Up,
    TpvApplicationInputPointerEventType.Motion:begin
     case aPointerEvent.PointerEventType of
      TpvApplicationInputPointerEventType.Down:begin
       case aPointerEvent.Button of
        TpvApplicationInputPointerButton.Left:begin
         fMouseButtons:=fMouseButtons or 1;
        end;
        TpvApplicationInputPointerButton.Right:begin
         fMouseButtons:=fMouseButtons or 2;
        end;
        TpvApplicationInputPointerButton.Middle:begin
         fMouseButtons:=fMouseButtons or 4;
        end;
        else begin
        end;
       end;
      end;
      TpvApplicationInputPointerEventType.Up:begin
       case aPointerEvent.Button of
        TpvApplicationInputPointerButton.Left:begin
         fMouseButtons:=fMouseButtons and not 1;
        end;
        TpvApplicationInputPointerButton.Right:begin
         fMouseButtons:=fMouseButtons and not 2;
        end;
        TpvApplicationInputPointerButton.Middle:begin
         fMouseButtons:=fMouseButtons and not 4;
        end;
        else begin
        end;
       end;
      end;
     end;
     if assigned(fMachineInstance.fMachine.VirtIOInputMouseDevice) then begin
      fMachineInstance.fMachine.VirtIOInputMouseDevice.HandleMouse(round(aPointerEvent.RelativePosition.x),round(aPointerEvent.RelativePosition.y),0,fMouseButtons);
     end;
    end;
    TpvApplicationInputPointerEventType.Drag:begin
    end;
   end;
  end;
 end;
end;

function TScreenEmulator.Scrolled(const aRelativeAmount:TpvVector2):boolean;
begin
 result:=false;
 if fReady and not fTerminalMode then begin
  if assigned(fMachineInstance.fMachine.PS2MouseDevice) then begin
   fMachineInstance.fMachine.PS2MouseDevice.Scroll(round(abs(aRelativeAmount.x)+abs(aRelativeAmount.y)));
  end;
  if assigned(fMachineInstance.fMachine.VirtIOInputMouseDevice) then begin
   fMachineInstance.fMachine.VirtIOInputMouseDevice.HandleMouse(0,0,round(abs(aRelativeAmount.x)+abs(aRelativeAmount.y)),fMouseButtons);
  end;
 end;
end;

function TScreenEmulator.CanBeParallelProcessed:boolean;
begin
 result:=true;
end;

procedure TScreenEmulator.Check(const aDeltaTime:TpvDouble);
begin
 inherited Check(aDeltaTime);
 if fMachineInstance.Finished then begin
  pvApplication.Terminate;
 end;
end;

function ConvertColor(const c:TPasTermUInt32):TpvVector4;
begin
 result.r:=((c shr 0) and $ff)/$ff;
 result.g:=((c shr 8) and $ff)/$ff;
 result.b:=((c shr 16) and $ff)/$ff;
 result.a:=1.0;
end;

procedure TScreenEmulator.Update(const aDeltaTime:TpvDouble);
var Index,SubIndex,Column,Row,IncomingChars,StartColumn,CountColumns:TpvInt32;
    StartColor,Color,OldColor:TPasTermUInt32;
    Scale,f:TpvFloat;
    fv:TpvVector4;
    Updated,FrameBufferActive,FrameBufferGenerationDirty:boolean;
begin

 inherited Update(aDeltaTime);

 if assigned(fMachineInstance) and
    assigned(fMachineInstance.fMachine) and
    assigned(fMachineInstance.fMachine.PS2KeyboardDevice) then begin
  fMachineInstance.fMachine.PS2KeyboardDevice.Update;
 end;

 if assigned(fMachineInstance) and
    assigned(fMachineInstance.fMachine) and
    assigned(fMachineInstance.fMachine.FrameBufferDevice) and
    fMachineInstance.fMachine.FrameBufferDevice.AutomaticRefresh and
    (fMachineInstance.fNextFrameTime<=pvApplication.HighResolutionTimer.GetTime) then begin
  if fMachineInstance.fMachine.FrameBufferDevice.CheckDirtyAndFlush then begin
   fMachineInstance.OnNewFrame;
  end;
  fMachineInstance.fNextFrameTime:=pvApplication.HighResolutionTimer.GetTime+(pvApplication.HighResolutionTimer.SecondInterval div 60);
 end;

 f:=1.0;
 fv:=TpvVector4.InlineableCreate(f,f,f,1.0);

 Updated:=false;
 FrameBufferGenerationDirty:=false;
 if fMachineInstance.TransferFrame(@fGraphicsFrameBuffer,FrameBufferActive) then begin
  if FrameBufferActive then begin
   FrameBufferGenerationDirty:=true;
  end else begin
   fTerm.Refresh;
  end;
 end;
 repeat
  IncomingChars:=fMachineInstance.fMachine.UARTDevice.OutputRingBuffer.AvailableForRead;
  if IncomingChars>0 then begin
   IncomingChars:=fMachineInstance.fMachine.UARTDevice.OutputRingBuffer.ReadAsMuchAsPossible(@fUARTOutputBuffer,Min(IncomingChars,SizeOf(fUARTOutputBuffer)));
   if IncomingChars>0 then begin
    system.write(copy(fUARTOutputBuffer,0,IncomingChars));
    fTerm.Write(@fUARTOutputBuffer,IncomingChars);
    Updated:=true;
   end;
  end else begin
   break;
  end;
 until false;
 if Updated then begin
  fTerminalFrameBufferSnapshot.Update;
  FrameBufferGenerationDirty:=true;
 end;
 if fLastTerminalMode<>fTerminalMode then begin
  fLastTerminalMode:=fTerminalMode;
  FrameBufferGenerationDirty:=true;
 end;
 if FrameBufferGenerationDirty then begin
  inc(fFrameBufferGeneration);
 end;
 if fFrameBufferGenerations[pvApplication.UpdateInFlightFrameIndex]<>fFrameBufferGeneration then begin
  fFrameBufferGenerations[pvApplication.UpdateInFlightFrameIndex]:=fFrameBufferGeneration;
  if fTerminalMode then begin
   fFrameBuffers[pvApplication.UpdateInFlightFrameIndex]:=fTerminalFrameBuffer;
  end else begin
   fFrameBuffers[pvApplication.UpdateInFlightFrameIndex]:=fGraphicsFrameBuffer;
  end;
 end;

 fVulkanCanvas.Start(pvApplication.UpdateInFlightFrameIndex);

 // Scaled to fit within the canvas while preserving its aspect ratio and centered, with possible black borders
 if (fVulkanCanvas.Width/fVulkanCanvas.Height)<(640.0/400.0) then begin
  Scale:=fVulkanCanvas.Width/640;
//fVulkanCanvas.ViewMatrix:=TpvMatrix4x4.CreateScale(Scale,Scale,1.0)*TpvMatrix4x4.CreateTranslation(0,(fVulkanCanvas.Height-(400*Scale))*0.5,0);
 end else begin
  Scale:=fVulkanCanvas.Height/400;
//fVulkanCanvas.ViewMatrix:=TpvMatrix4x4.CreateScale(Scale,Scale,1.0)*TpvMatrix4x4.CreateTranslation((fVulkanCanvas.Width-(640*Scale))*0.5,0,0);
 end;
 // More unified way to scale for also to include a possible additional scaling factor on top of the aspect ratio preserving scaling and
 // optional flipping of the axes (for example for rendering to a texture) as well as a translation to center the content.
 fVulkanCanvas.ViewMatrix:=TpvMatrix4x4.CreateTranslation(-TMachineInstance.ScreenWidth*0.5,-TMachineInstance.ScreenHeight*0.5,0)*
                           TpvMatrix4x4.CreateScale(Scale,Scale,1.0)*
                           TpvMatrix4x4.CreateTranslation(fVulkanCanvas.Width*0.5,fVulkanCanvas.Height*0.5,0);

 fVulkanCanvas.BlendingMode:=TpvCanvasBlendingMode.None;
 fVulkanCanvas.Color:=ConvertSRGBToLinear(TpvVector4.Create(1.0,1.0,1.0,1.0));
 fVulkanCanvas.DrawTexturedRectangle(fFrameBufferTextures[pvApplication.UpdateInFlightFrameIndex],
                                     TpvRect.CreateAbsolute(0.0,0.0,TMachineInstance.ScreenWidth,TMachineInstance.ScreenHeight));

{fVulkanCanvas.BlendingMode:=TpvCanvasBlendingMode.AlphaBlending;

 fVulkanCanvas.Color:=ConvertSRGBToLinear(TpvVector4.Create(1.0,1.0,1.0,1.0));

 fVulkanCanvas.Font:=fVulkanFont;

 fVulkanCanvas.FontSize:=-16;

 OldColor:=High(TPasTermUInt32);
 Index:=0;
 for Row:=0 to fTerminalFrameBufferSnapshot.Rows-1 do begin
  StartColor:=High(TPasTermUInt32);
  StartColumn:=0;
  CountColumns:=0;
  for Column:=0 to fTerminalFrameBufferSnapshot.Columns-1 do begin
   if fTerminalFrameBufferSnapshot.CursorEnabled and (Column=fTerminalFrameBufferSnapshot.CursorColumn) and (Row=fTerminalFrameBufferSnapshot.CursorRow) then begin
    Color:=fTerminalFrameBufferSnapshot.Content[Index].ForegroundColor;
   end else begin
    Color:=fTerminalFrameBufferSnapshot.Content[Index].BackgroundColor;
   end;
   if (StartColor=Color) and (CountColumns>0) then begin
    inc(CountColumns);
   end else begin
    if (CountColumns>0) and (StartColor<>0) then begin
     if OldColor<>StartColor then begin
      OldColor:=StartColor;
      fVulkanCanvas.Color:=ConvertSRGBToLinear(ConvertColor(StartColor))*fv;
     end;
     fVulkanCanvas.DrawFilledRectangle(TpvRect.CreateAbsolute(8*StartColumn,16*Row,8*(StartColumn+CountColumns),(16*Row)+16));
    end;
    StartColor:=Color;
    StartColumn:=Column;
    CountColumns:=1;
   end; 
   inc(Index);
  end;
  if (CountColumns>0) and (StartColor<>0) then begin
   if OldColor<>StartColor then begin
    OldColor:=StartColor;
    fVulkanCanvas.Color:=ConvertSRGBToLinear(ConvertColor(StartColor))*fv;
   end;
   fVulkanCanvas.DrawFilledRectangle(TpvRect.CreateAbsolute(8*StartColumn,16*Row,8*(StartColumn+CountColumns),(16*Row)+16));
  end;
 end;

 Index:=0;
 for Row:=0 to fTerminalFrameBufferSnapshot.Rows-1 do begin
  for Column:=0 to fTerminalFrameBufferSnapshot.Columns-1 do begin
   if fTerminalFrameBufferSnapshot.Content[Index].CodePoint<>32 then begin
    if fTerminalFrameBufferSnapshot.CursorEnabled and (Column=fTerminalFrameBufferSnapshot.CursorColumn) and (Row=fTerminalFrameBufferSnapshot.CursorRow) then begin
     Color:=fTerminalFrameBufferSnapshot.Content[Index].BackgroundColor;
    end else begin
     Color:=fTerminalFrameBufferSnapshot.Content[Index].ForegroundColor;
    end;
    if OldColor<>Color then begin
     OldColor:=Color;
     fVulkanCanvas.Color:=ConvertSRGBToLinear(ConvertColor(Color))*fv;
    end;
    fVulkanCanvas.DrawTextCodePoint(fTerminalFrameBufferSnapshot.Content[Index].CodePoint,8*Column,16*Row);
   end;
   inc(Index);
  end;
 end;//}

 fVulkanCanvas.Stop;

 fTime:=fTime+aDeltaTime;

 fReady:=true;
end;

procedure TScreenEmulator.Draw(const aSwapChainImageIndex:TpvInt32;var aWaitSemaphore:TpvVulkanSemaphore;const aWaitFence:TpvVulkanFence=nil);
const Offsets:array[0..0] of TVkDeviceSize=(0);
var VulkanCommandBuffer:TpvVulkanCommandBuffer;
    VulkanSwapChain:TpvVulkanSwapChain;
begin

 begin

  begin

   if fFrameBufferTextureGenerations[pvApplication.DrawInFlightFrameIndex]<>fFrameBufferGenerations[pvApplication.DrawInFlightFrameIndex] then begin
    fFrameBufferTextureGenerations[pvApplication.DrawInFlightFrameIndex]:=fFrameBufferGenerations[pvApplication.DrawInFlightFrameIndex];
    fFrameBufferTextures[pvApplication.DrawInFlightFrameIndex].Upload(pvApplication.VulkanDevice.GraphicsQueue,
                                                                      fVulkanGraphicsCommandBuffer,
                                                                      fVulkanGraphicsCommandBufferFence,
                                                                      pvApplication.VulkanDevice.TransferQueue,
                                                                      fVulkanTransferCommandBuffer,
                                                                      fVulkanTransferCommandBufferFence,
                                                                      @fFrameBuffers[pvApplication.DrawInFlightFrameIndex][0],
                                                                      SizeOf(TMachineInstance.TFrameBuffer),
                                                                      false,
                                                                      false,
                                                                      0,
                                                                      true,
                                                                      nil,
                                                                      true);
   end;

   VulkanCommandBuffer:=fVulkanRenderCommandBuffers[pvApplication.DrawInFlightFrameIndex];
   VulkanSwapChain:=pvApplication.VulkanSwapChain;

   VulkanCommandBuffer.Reset(TVkCommandBufferResetFlags(VK_COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT));

   VulkanCommandBuffer.BeginRecording(TVkCommandBufferUsageFlags(VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT));

   fVulkanCanvas.ExecuteUpload(pvApplication.VulkanDevice.TransferQueue,
                               fVulkanTransferCommandBuffer,
                               fVulkanTransferCommandBufferFence,
                               pvApplication.DrawInFlightFrameIndex);

   fVulkanRenderPass.BeginRenderPass(VulkanCommandBuffer,
                                     pvApplication.VulkanFrameBuffers[aSwapChainImageIndex],
                                     VK_SUBPASS_CONTENTS_INLINE,
                                     0,
                                     0,
                                     VulkanSwapChain.Width,
                                     VulkanSwapChain.Height);

   fVulkanCanvas.ExecuteDraw(VulkanCommandBuffer,
                             pvApplication.DrawInFlightFrameIndex);

   fVulkanRenderPass.EndRenderPass(VulkanCommandBuffer);

   VulkanCommandBuffer.EndRecording;

   VulkanCommandBuffer.Execute(pvApplication.VulkanDevice.GraphicsQueue,
                               TVkPipelineStageFlags(VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT),
                               aWaitSemaphore,
                               fVulkanRenderSemaphores[pvApplication.DrawInFlightFrameIndex],
                               aWaitFence,
                               false);

   aWaitSemaphore:=fVulkanRenderSemaphores[pvApplication.DrawInFlightFrameIndex];

  end;
 end;

end;

initialization
end.
