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

{$rangechecks off}
{$overflowchecks off}

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
     PasVulkan.PasRISCVEmulator,
     PasRISCV,
     PasTerm,
     VGAFont;

type { TMachineInstance }
     TMachineInstance=class(TpvPasRISCVEmulatorMachineInstance)
      public
       const CanvasWidth=ScreenWidth*4;
             CanvasHeight=ScreenHeight*4;
      protected
       procedure ConfigureMachine; override;
       function GetBIOSFileName:TpvRawByteString; override;
       function GetKernelFileName:TpvRawByteString; override;
       function GetINITRDFileName:TpvRawByteString; override;
       function GetVirtIOBlockImageFileName:TpvRawByteString; override;
       function GetNVMeImageFileName:TpvRawByteString; override;
       procedure PreBoot; override;
       procedure Execute; override;
     end;

     { TScreenEmulator }

     TScreenEmulator=class(TpvApplicationScreen)
      private
       fRenderer:TpvPasRISCVEmulatorRenderer;
       fVulkanRenderPass:TpvVulkanRenderPass;
       fVulkanCommandPool:TpvVulkanCommandPool;
       fVulkanRenderCommandBuffers:array[0..MaxInFlightFrames-1] of TpvVulkanCommandBuffer;
       fVulkanRenderSemaphores:array[0..MaxInFlightFrames-1] of TpvVulkanSemaphore;
       fMachineInstance:TMachineInstance;
       fReady:boolean;
       fTime:TpvDouble;
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

{ TMachineInstance }

procedure TMachineInstance.ConfigureMachine;
begin
 fMachineConfiguration.Debugger:=(Application.DebuggerPort>=0) or Application.DebuggerLocal;
 fMachineConfiguration.DebuggerLocal:=Application.DebuggerLocal;
 fMachineConfiguration.DebuggerPort:=Application.DebuggerPort;
 fMachineConfiguration.CountHARTs:=Application.CountHARTs;
 fMachineConfiguration.BootArguments:=Application.BootArguments;
 fMachineConfiguration.MemorySize:=Application.MemorySize;
 fMachineConfiguration.AIA:=Application.AIA;
 fMachineConfiguration.DisplayMode:=Application.DisplayMode;
end;

function TMachineInstance.GetBIOSFileName:TpvRawByteString;
begin
 result:=Application.BIOSFileName;
end;

function TMachineInstance.GetKernelFileName:TpvRawByteString;
begin
 result:=Application.KernelFileName;
end;

function TMachineInstance.GetINITRDFileName:TpvRawByteString;
begin
 result:=Application.InitrdFileName;
end;

function TMachineInstance.GetVirtIOBlockImageFileName:TpvRawByteString;
begin
 result:=Application.VirtIOBlockImageFileName;
end;

function TMachineInstance.GetNVMeImageFileName:TpvRawByteString;
begin
 result:=Application.NVMeImageFileName;
end;

procedure TMachineInstance.PreBoot;
begin
 if fMachineConfiguration.VirtIOBlockEnabled then begin
  fMachine.VirtIOBlockDevice.AttachStream(nil);
 end;
 if fMachineConfiguration.NVMeEnabled then begin
  fMachine.NVMeDevice.AttachStream(nil);
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
end;

{ TScreenEmulator }

constructor TScreenEmulator.Create;
begin
 inherited Create;
 fRenderer:=nil;
 fMachineInstance:=nil;
 fReady:=false;
 fTime:=0.48;
end;

destructor TScreenEmulator.Destroy;
begin
 inherited Destroy;
end;

procedure TScreenEmulator.Show;
var Index:TpvInt32;
begin
 inherited Show;

 fRenderer:=TpvPasRISCVEmulatorRenderer.Create;
 fRenderer.CreateVulkanResources(pvApplication.VulkanDevice);

 fVulkanCommandPool:=TpvVulkanCommandPool.Create(pvApplication.VulkanDevice,
                                                 pvApplication.VulkanDevice.GraphicsQueueFamilyIndex,
                                                 TVkCommandPoolCreateFlags(VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT));
 for Index:=0 to MaxInFlightFrames-1 do begin
  fVulkanRenderCommandBuffers[Index]:=TpvVulkanCommandBuffer.Create(fVulkanCommandPool,VK_COMMAND_BUFFER_LEVEL_PRIMARY);
  fVulkanRenderSemaphores[Index]:=TpvVulkanSemaphore.Create(pvApplication.VulkanDevice);
 end;

 fVulkanRenderPass:=nil;

 fMachineInstance:=TMachineInstance.Create;

 fRenderer.SetMachineInstance(fMachineInstance);
end;

procedure TScreenEmulator.Hide;
var Index:TpvInt32;
begin
 fRenderer.ShutdownMachineInstance;
 fMachineInstance:=nil;

 FreeAndNil(fVulkanRenderPass);
 for Index:=0 to MaxInFlightFrames-1 do begin
  FreeAndNil(fVulkanRenderCommandBuffers[Index]);
  FreeAndNil(fVulkanRenderSemaphores[Index]);
 end;
 FreeAndNil(fVulkanCommandPool);

 fRenderer.DestroyVulkanResources;
 FreeAndNil(fRenderer);

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
                                                                                                                              VK_IMAGE_LAYOUT_UNDEFINED,
                                                                                                                              VK_IMAGE_LAYOUT_PRESENT_SRC_KHR
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
                                                                                                                             VK_IMAGE_LAYOUT_UNDEFINED,
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

 fRenderer.VulkanCanvas.VulkanRenderPass:=fVulkanRenderPass;
 fRenderer.VulkanCanvas.CountBuffers:=pvApplication.CountInFlightFrames;
 fRenderer.VulkanCanvas.Width:=pvApplication.Width;
 fRenderer.VulkanCanvas.Height:=pvApplication.Height;
 fRenderer.VulkanCanvas.Viewport.x:=0;
 fRenderer.VulkanCanvas.Viewport.y:=0;
 fRenderer.VulkanCanvas.Viewport.width:=pvApplication.Width;
 fRenderer.VulkanCanvas.Viewport.height:=pvApplication.Height;

 for Index:=0 to length(fVulkanRenderCommandBuffers)-1 do begin
  FreeAndNil(fVulkanRenderCommandBuffers[Index]);
  fVulkanRenderCommandBuffers[Index]:=TpvVulkanCommandBuffer.Create(fVulkanCommandPool,VK_COMMAND_BUFFER_LEVEL_PRIMARY);
 end;

end;

procedure TScreenEmulator.BeforeDestroySwapChain;
begin
 fRenderer.VulkanCanvas.VulkanRenderPass:=nil;
 FreeAndNil(fVulkanRenderPass);
 inherited BeforeDestroySwapChain;
end;

function TScreenEmulator.KeyEvent(const aKeyEvent:TpvApplicationInputKeyEvent):boolean;
begin
 result:=fRenderer.HandleKeyEvent(aKeyEvent);
end;

function TScreenEmulator.PointerEvent(const aPointerEvent:TpvApplicationInputPointerEvent):boolean;
begin
 result:=fRenderer.HandlePointerEvent(aPointerEvent);
end;

function TScreenEmulator.Scrolled(const aRelativeAmount:TpvVector2):boolean;
begin
 result:=fRenderer.HandleScrolled(aRelativeAmount);
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

procedure TScreenEmulator.Update(const aDeltaTime:TpvDouble);
begin

 inherited Update(aDeltaTime);

 fRenderer.UpdateEmulatorState;

 fRenderer.RenderToCanvas(pvApplication.UpdateInFlightFrameIndex,
                          fRenderer.VulkanCanvas.Width,
                          fRenderer.VulkanCanvas.Height);

 fTime:=fTime+aDeltaTime;

 fReady:=true;
 fRenderer.Ready:=true;
end;

procedure TScreenEmulator.Draw(const aSwapChainImageIndex:TpvInt32;var aWaitSemaphore:TpvVulkanSemaphore;const aWaitFence:TpvVulkanFence=nil);
const Offsets:array[0..0] of TVkDeviceSize=(0);
var VulkanCommandBuffer:TpvVulkanCommandBuffer;
    VulkanSwapChain:TpvVulkanSwapChain;
begin

 begin

  begin

   VulkanCommandBuffer:=fVulkanRenderCommandBuffers[pvApplication.DrawInFlightFrameIndex];
   VulkanSwapChain:=pvApplication.VulkanSwapChain;

   VulkanCommandBuffer.Reset(TVkCommandBufferResetFlags(VK_COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT));

   VulkanCommandBuffer.BeginRecording(TVkCommandBufferUsageFlags(VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT));

   fRenderer.RecordFrameBufferUpload(VulkanCommandBuffer,pvApplication.DrawInFlightFrameIndex);

   fRenderer.VulkanCanvas.ExecuteUpload(fRenderer.VulkanDevice.TransferQueue,
                                        fRenderer.VulkanTransferCommandBuffer,
                                        fRenderer.VulkanTransferCommandBufferFence,
                                        pvApplication.DrawInFlightFrameIndex);

   fVulkanRenderPass.BeginRenderPass(VulkanCommandBuffer,
                                     pvApplication.VulkanFrameBuffers[aSwapChainImageIndex],
                                     VK_SUBPASS_CONTENTS_INLINE,
                                     0,
                                     0,
                                     VulkanSwapChain.Width,
                                     VulkanSwapChain.Height);

   fRenderer.VulkanCanvas.ExecuteDraw(VulkanCommandBuffer,
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
