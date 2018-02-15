unit uOMX;

{$mode delphi}{$H+}
(*
 * Copyright (c) 2008 The Khronos Group Inc.
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject
 * to the following conditions:
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
 * OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
 * CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
 * TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
 * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 *
 *)

interface

uses
  Classes, SysUtils, VC4;

const
  OMX_MAX_STRINGNAME_SIZE                  = 128;
  OMX_BUFFERFLAG_EOS                       = $00000001;
  OMX_BUFFERFLAG_STARTTIME                 = $00000002;
  OMX_BUFFERFLAG_DECODEONLY                = $00000004;
  OMX_BUFFERFLAG_DATACORRUPT               = $00000008;
  OMX_BUFFERFLAG_ENDOFFRAME                = $00000010;
  OMX_BUFFERFLAG_SYNCFRAME                 = $00000020;
  OMX_BUFFERFLAG_EXTRADATA                 = $00000040;
  OMX_BUFFERFLAG_CODECCONFIG               = $00000080;

  OMX_BUFFERFLAG_TIME_UNKNOWN              = $00000100;
  OMX_BUFFERFLAG_ENDOFNAL                  = $00000400;
  OMX_BUFFERFLAG_FRAGMENTLIST              = $00000800;
  OMX_BUFFERFLAG_DISCONTINUITY             = $00001000;
  OMX_BUFFERFLAG_CODECSIDEINFO             = $00002000;


type
  OMX_U8                                   = uint8;
  POMX_U8                                  = ^OMX_U8;
  PPOMX_U8                                 = ^POMX_U8;
  OMX_S8                                   = Int8;
  OMX_U16                                  = uint16;
  OMX_S16                                  = Int16;
  OMX_U32                                  = uint32;
  POMX_U32                                 = ^OMX_U32;
  OMX_S32                                  = integer;
  OMX_U64                                  = uint64;
  OMX_S64                                  = Int64;
  OMX_BOOL                                 = LongBool;
  OMX_PTR                                  = pointer;
  OMX_STRING                               = PChar;

  OMX_INDEXTYPE                            = OMX_U32;  // see constants
  POMX_INDEXTYPE                           = ^OMX_INDEXTYPE;

  OMX_HANDLETYPE                           = pointer;
  POMX_HANDLETYPE                          = ^OMX_HANDLETYPE;
  OMX_NATIVE_DEVICETYPE                    = pointer;
  OMX_NATIVE_WINDOWTYPE                    = pointer;
  OMX_UUIDTYPE                             = array [0 .. 127] of byte;
  POMX_UUIDTYPE                            = ^OMX_UUIDTYPE;

  OMX_ENDIANTYPE                           = LongWord;
  OMX_NUMERICALDATATYPE                    = LongWord;

  {$PACKRECORDS C}

  OMX_MARKTYPE = record
    hMarkTargetComponent : OMX_HANDLETYPE;   (* The component that will
                                                generate a mark event upon
                                                processing the mark. *)
    pMarkData : OMX_PTR;                     (* Application specific data associated with
                                                the mark sent on a mark event to disambiguate
                                                this mark from others. *)
  end;



type
  OMX_PORTDOMAINTYPE                       = Longword;

const
  OMX_PortDomainAudio                      = 0;
  OMX_PortDomainVideo                      = 1;
  OMX_PortDomainImage                      = 2;
  OMX_PortDomainOther                      = 3;
  OMX_PortDomainKhronosExtensions          = $6F000000; (* Reserved region for introducing Khronos Standard Extensions *)
  OMX_PortDomainVendorStartUnused          = $7F000000; (* Reserved region for introducing Vendor Extensions *)
  OMX_PortDomainMax                        = $7ffffff;

type
  OMX_ERRORTYPE                            = Longword;

const
  OMX_ErrorNone                            = 0;
  (* There were insufficient resources to perform the requested operation *)
  OMX_ErrorInsufficientResources           = $80001000;
  (* There was an error, but the cause of the error could not be determined *)
  OMX_ErrorUndefined                       = $80001001;
  (* The component name string was not valid *)
  OMX_ErrorInvalidComponentName            = $80001002;
  (* No component with the specified name string was found *)
  OMX_ErrorComponentNotFound               = $80001003;
  (* The component specified did not have a "OMX_ComponentInit" or
      "OMX_ComponentDeInit entry point *)
  OMX_ErrorInvalidComponent                = $80001004;
  (* One or more parameters were not valid *)
  OMX_ErrorBadParameter                    = $80001005;
  (* The requested function is not implemented *)
  OMX_ErrorNotImplemented                  = $80001006;
  (* The buffer was emptied before the next buffer was ready *)
  OMX_ErrorUnderflow                       = $80001007;
  (* The buffer was not available when it was needed *)
  OMX_ErrorOverflow                        = $80001008;
  (* The hardware failed to respond as expected *)
  OMX_ErrorHardware                        = $80001009;
  (* The component is in the state OMX_StateInvalid *)
  OMX_ErrorInvalidState                    = $8000100A;
  (* Stream is found to be corrupt *)
  OMX_ErrorStreamCorrupt                   = $8000100B;
  (* Ports being connected are not compatible *)
  OMX_ErrorPortsNotCompatible              = $8000100C;
  (* Resources allocated to an idle component have been
      lost resulting in the component returning to the loaded state *)
  OMX_ErrorResourcesLost                   = $8000100D;
  (* No more indicies can be enumerated *)
  OMX_ErrorNoMore                          = $8000100E;
  (* The component detected a version mismatch *)
  OMX_ErrorVersionMismatch                 = $8000100F;
  (* The component is not ready to return data at this time *)
  OMX_ErrorNotReady                        = $80001010;
  (* There was a timeout that occurred *)
  OMX_ErrorTimeout                         = $80001011;
  (* This error occurs when trying to transition into the state you are already in *)
  OMX_ErrorSameState                       = $80001012;
  (* Resources allocated to an executing or paused component have been
      preempted, causing the component to return to the idle state *)
  OMX_ErrorResourcesPreempted              = $80001013;
  (* A non-supplier port sends this error to the IL client (via the EventHandler callback)
      during the allocation of buffers (on a transition from the LOADED to the IDLE state or
      on a port restart) when it deems that it has waited an unusually long time for the supplier
      to send it an allocated buffer via a UseBuffer call. *)
  OMX_ErrorPortUnresponsiveDuringAllocation = $80001014;
  (* A non-supplier port sends this error to the IL client (via the EventHandler callback)
      during the deallocation of buffers (on a transition from the IDLE to LOADED state or
      on a port stop) when it deems that it has waited an unusually long time for the supplier
      to request the deallocation of a buffer header via a FreeBuffer call. *)
  OMX_ErrorPortUnresponsiveDuringDeallocation = $80001015;
  (* A supplier port sends this error to the IL client (via the EventHandler callback)
      during the stopping of a port (either on a transition from the IDLE to LOADED
      state or a port stop) when it deems that it has waited an unusually long time for
      the non-supplier to return a buffer via an EmptyThisBuffer or FillThisBuffer call. *)
  OMX_ErrorPortUnresponsiveDuringStop      = $80001016;
  (* Attempting a state transtion that is not allowed *)
  OMX_ErrorIncorrectStateTransition        = $80001017;
  (* Attempting a command that is not allowed during the present state. *)
  OMX_ErrorIncorrectStateOperation         = $80001018;
  (* The values encapsulated in the parameter or config structure are not supported. *)
  OMX_ErrorUnsupportedSetting              = $80001019;
  (* The parameter or config indicated by the given index is not supported. *)
  OMX_ErrorUnsupportedIndex                = $8000101A;
  (* The port index supplied is incorrect. *)
  OMX_ErrorBadPortIndex                    = $8000101B;
  (* The port has lost one or more of its buffers and it thus unpopulated. *)
  OMX_ErrorPortUnpopulated                 = $8000101C;
  (* Component suspended due to temporary loss of resources *)
  OMX_ErrorComponentSuspended              = $8000101D;
  (* Component suspended due to an inability to acquire dynamic resources *)
  OMX_ErrorDynamicResourcesUnavailable     = $8000101E;
  (* When the macroblock error reporting is enabled the component returns new error
  for every frame that has errors *)
  OMX_ErrorMbErrorsInFrame                 = $8000101F;
  (* A component reports this error when it cannot parse or determine the format of an input stream. *)
  OMX_ErrorFormatNotDetected               = $80001020;
  (* The content open operation failed. *)
  OMX_ErrorContentPipeOpenFailed           = $80001021;
  (* The content creation operation failed. *)
  OMX_ErrorContentPipeCreationFailed       = $80001022;
  (* Separate table information is being used *)
  OMX_ErrorSeperateTablesUsed              = $80001023;
  (* Tunneling is unsupported by the component*)
  OMX_ErrorTunnelingUnsupported            = $80001024;
  (* Reserved region for introducing Khronos Standard Extensions *)
  OMX_ErrorKhronosExtensions               = $8F000000;
  (* Reserved region for introducing Vendor Extensions *)
  OMX_ErrorVendorStartUnused               = $90000000;
  (* Disk Full error *)
  OMX_ErrorDiskFull                        = $90000001;
  (* Max file size is reached *)
  OMX_ErrorMaxFileSize                     = $90000002;
  (* Unauthorised to play a DRM protected file *)
  OMX_ErrorDrmUnauthorised                 = $90000003;
  (* The DRM protected file has expired *)
  OMX_ErrorDrmExpired                      = $90000004;
  (* Some other DRM library error *)
  OMX_ErrorDrmGeneral                      = $90000005;
  OMX_ErrorMax                             = $7FFFFFFF;

type
  OMX_VERSIONTYPE = record
    case boolean of
      false :
        (
          nVersionMajor : OMX_U8;   (* Major version accessor element *)
          nVersionMinor : OMX_U8;   (* Minor version accessor element *)
          nRevision : OMX_U8;       (* Revision version accessor element *)
          nStep : OMX_U8;           (* Step version accessor element *)
        );
      true :
        (
          nVersion : OMX_U32;       (* 32 bit value to make accessing the
                                       version easily done in a single word
                                       size copy/compare operation *)
        )
  end;
  POMX_VERSIONTYPE = ^OMX_VERSIONTYPE;

  OMX_PORT_PARAM_TYPE = record
    nSize : OMX_U32;                                     (* size of the structure in bytes *)
    nVersion : OMX_VERSIONTYPE;                          (* OMX specification version information *)
    nPorts : OMX_U32;                                    (* The number of ports for this component *)
    nStartPortNumber : OMX_U32;                          (* first port number for this type of port *)
  end;
  POMX_PORT_PARAM_TYPE = ^OMX_PORT_PARAM_TYPE;



type
  OMX_EVENTTYPE = LongWord;

const
  OMX_EventCmdComplete                     = 0;          (* component has sucessfully completed a command *)
  OMX_EventError                           = 1;          (* component has detected an error condition *)
  OMX_EventMark                            = 2;          (* component has detected a buffer mark *)
  OMX_EventPortSettingsChanged             = 3;          (* component is reported a port settings change *)
  OMX_EventBufferFlag                      = 4;          (* component has detected an EOS *)
  OMX_EventResourcesAcquired               = 5;          (* component has been granted resources and is
                                                            automatically starting the state change from
                                                            OMX_StateWaitForResources to OMX_StateIdle. *)
  OMX_EventComponentResumed                = 6;          (* Component resumed due to reacquisition of resources *)
  OMX_EventDynamicResourcesAvailable       = 7;          (* Component has acquired previously unavailable dynamic resources *)
  OMX_EventPortFormatDetected              = 8;          (* Component has detected a supported format. *)
  OMX_EventKhronosExtensions               = $6F000000;  (* Reserved region for introducing Khronos Standard Extensions *)
  OMX_EventVendorStartUnused               = $7F000000;  (* Reserved region for introducing Vendor Extensions *)
  OMX_EventParamOrConfigChanged            = $7F000001;  (* Should be added to the main spec as part of IL416c *)
  OMX_EventMax                             = $7FFFFFFF;

type
  OMX_TICKS = record
    nLowPart : OMX_U32;                    (* low bits of the signed 64 bit tick value *)
    nHighPart : OMX_U32;                   (* high bits of the signed 64 bit tick value *)
  end;
  POMX_TICKS = ^OMX_TICKS;

  OMX_BUFFERHEADERTYPE = record
    nSize : OMX_U32;                       (* size of the structure in bytes *)
    nVersion : OMX_VERSIONTYPE;            (* OMX specification version information *)
    pBuffer : POMX_U8;                     (* Pointer to actual block of memory
                                              that is acting as the buffer *)
    nAllocLen : OMX_U32;                   (* size of the buffer allocated, in bytes *)
    nFilledLen : OMX_U32;                  (* number of bytes currently in the buffer *)
    nOffset : OMX_U32;                     (* start offset of valid data in bytes from
                                              the start of the buffer *)
    pAppPrivate : OMX_PTR;                 (* pointer to any data the application
                                              wants to associate with this buffer *)
    pPlatformPrivate  : OMX_PTR;           (* pointer to any data the platform
                                              wants to associate with this buffer *)
    pInputPortPrivate : OMX_PTR;           (* pointer to any data the input port
                                              wants to associate with this buffer *)
    pOutputPortPrivate  : OMX_PTR;         (* pointer to any data the output port
                                              wants to associate with this buffer *)
    hMarkTargetComponent : OMX_HANDLETYPE; (* The component that will generate a
                                              mark event upon processing this buffer. *)
    pMarkData : OMX_PTR;                   (* Application specific data associated with
                                              the mark sent on a mark event to disambiguate
                                              this mark from others. *)
    nTickCount : OMX_U32;                  (* Optional entry that the component and
                                              application can update with a tick count
                                              when they access the component.  This
                                              value should be in microseconds.  Since
                                              this is a value relative to an arbitrary
                                              starting point, this value cannot be used
                                              to determine absolute time.  This is an
                                              optional entry and not all components
                                              will update it.*)
    nTimeStamp : OMX_TICKS;                (* Timestamp corresponding to the sample
                                              starting at the first logical sample
                                              boundary in the buffer. Timestamps of
                                              successive samples within the buffer may
                                              be inferred by adding the duration of the
                                              of the preceding buffer to the timestamp
                                              of the preceding buffer.*)
    nFlags : OMX_U32;                      (* buffer specific flags *)
    nOutputPortIndex : OMX_U32;            (* The index of the output port (if any) using
                                              this buffer *)
    nInputPortIndex : OMX_U32;             (* The index of the input port (if any) using
                                              this buffer *)
  end;
  POMX_BUFFERHEADERTYPE                    = ^OMX_BUFFERHEADERTYPE;
  PPOMX_BUFFERHEADERTYPE                   = ^POMX_BUFFERHEADERTYPE;



  TEventHandler = function (hComponent : OMX_HANDLETYPE;
                            pAppData : OMX_PTR;
                            eEvent : OMX_EVENTTYPE;
                            nData1 : OMX_U32;
                            nData2 : OMX_U32;
                            pEventData : OMX_PTR) : OMX_ERRORTYPE; cdecl;

  TFillBufferDone = function (hComponent : OMX_HANDLETYPE;
                              pAppData : OMX_PTR;
                              pBuffer : POMX_BUFFERHEADERTYPE) : OMX_ERRORTYPE; cdecl;

  TEmptyBufferDone = function (hComponent : OMX_HANDLETYPE;
                               pAppData : OMX_PTR;
                               pBuffer : POMX_BUFFERHEADERTYPE) : OMX_ERRORTYPE; cdecl;


  OMX_CALLBACKTYPE = record
    EventHandler : TEventHandler;

    (* The EventHandler method is used to notify the application when an
        event of interest occurs.  Events are defined in the OMX_EVENTTYPE
        enumeration.  Please see that enumeration for details of what will
        be returned for each type of event. Callbacks should not return
        an error to the component, so if an error occurs, the application
        shall handle it internally.  This is a blocking call.

        The application should return from this call within 5 msec to avoid
        blocking the component for an excessively long period of time.

        @param hComponent
            handle of the component to access.  This is the component
            handle returned by the call to the GetHandle function.
        @param pAppData
            pointer to an application defined value that was provided in the
            pAppData parameter to the OMX_GetHandle method for the component.
            This application defined value is provided so that the application
            can have a component specific context when receiving the callback.
        @param eEvent
            Event that the component wants to notify the application about.
        @param nData1
            nData will be the OMX_ERRORTYPE for an error event and will be
            an OMX_COMMANDTYPE for a command complete event and OMX_INDEXTYPE for a OMX_PortSettingsChanged event.
         @param nData2
            nData2 will hold further information related to the event. Can be OMX_STATETYPE for
            a OMX_CommandStateSet command or port index for a OMX_PortSettingsChanged event.
            Default value is 0 if not used. )
        @param pEventData
            Pointer to additional event-specific data (see spec for meaning).
      *)

    EmptyBufferDone : TEmptyBufferDone;
    (* The EmptyBufferDone method is used to return emptied buffers from an
        input port back to the application for reuse.  This is a blocking call
        so the application should not attempt to refill the buffers during this
        call, but should queue them and refill them in another thread.  There
        is no error return, so the application shall handle any errors generated
        internally.

        The application should return from this call within 5 msec.

        @param hComponent
            handle of the component to access.  This is the component
            handle returned by the call to the GetHandle function.
        @param pAppData
            pointer to an application defined value that was provided in the
            pAppData parameter to the OMX_GetHandle method for the component.
            This application defined value is provided so that the application
            can have a component specific context when receiving the callback.
        @param pBuffer
            pointer to an OMX_BUFFERHEADERTYPE structure allocated with UseBuffer
            or AllocateBuffer indicating the buffer that was emptied.
        @ingroup buf
     *)

    FillBufferDone : TFillBufferDone;
    (* The FillBufferDone method is used to return filled buffers from an
        output port back to the application for emptying and then reuse.
        This is a blocking call so the application should not attempt to
        empty the buffers during this call, but should queue the buffers
        and empty them in another thread.  There is no error return, so
        the application shall handle any errors generated internally.  The
        application shall also update the buffer header to indicate the
        number of bytes placed into the buffer.

        The application should return from this call within 5 msec.

        @param hComponent
            handle of the component to access.  This is the component
            handle returned by the call to the GetHandle function.
        @param pAppData
            pointer to an application defined value that was provided in the
            pAppData parameter to the OMX_GetHandle method for the component.
            This application defined value is provided so that the application
            can have a component specific context when receiving the callback.
        @param pBuffer
            pointer to an OMX_BUFFERHEADERTYPE structure allocated with UseBuffer
            or AllocateBuffer indicating the buffer that was filled.
        @ingroup buf  *)
  end;
  POMX_CALLBACKTYPE                        = ^OMX_CALLBACKTYPE;

  OMX_BUFFERSUPPLIERTYPE = Longword;


  OMX_TUNNELSETUPTYPE = record
    nTunnelFlags : OMX_U32;                  (* bit flags for tunneling *)
    eSupplier : OMX_BUFFERSUPPLIERTYPE;      (* supplier preference *)
  end;
  POMX_TUNNELSETUPTYPE                     = ^OMX_TUNNELSETUPTYPE;

const
  OMX_FALSE                                = LongBool (false);
  OMX_TRUE                                 = LongBool (true);

  OMX_NumericalDataSigned                  = 0; (* signed data *)
  OMX_NumericalDataUnsigned                = 1; (* unsigned data *)
  OMX_NumercialDataMax                     = $7FFFFFFF;

  OMX_EndianBig                            = 0; (* big endian *)
  OMX_EndianLittle                         = 1; (* little endian *)
  OMX_EndianMax                            = $7FFFFFFF;

  OMX_VERSION_MAJOR                        = 1;
  OMX_VERSION_MINOR                        = 1;
  OMX_VERSION_REVISION                     = 2;
  OMX_VERSION_STEP                         = 0;
  OMX_VERSION                              = (OMX_VERSION_STEP shl 24) or (OMX_VERSION_REVISION shl 16) or
                                             (OMX_VERSION_MINOR shl 8) or OMX_VERSION_MAJOR;



  OMX_BufferSupplyUnspecified              = 0;          (* port supplying the buffers is unspecified,
                                                            or don't care *)
  OMX_BufferSupplyInput                    = 1;          (* input port supplies the buffers *)
  OMX_BufferSupplyOutput                   = 2;          (* output port supplies the buffers *)
  OMX_BufferSupplyKhronosExtensions        = $6F000000;  (* Reserved region for introducing Khronos Standard Extensions *)
  OMX_BufferSupplyVendorStartUnused        = $7F000000;  (* Reserved region for introducing Vendor Extensions *)
  OMX_BufferSupplyMax                      = $7FFFFFFF;





type
  OMX_DIRTYPE = Longword;

const
  OMX_DirInput                             = 0;         (* Port is an input port *)
  OMX_DirOutput                            = 1;         (* Port is an output port *)
  OMX_DirMax                               = $7FFFFFFF;

type
  OMX_COMMANDTYPE = Longword;

const
  OMX_CommandStateSet                      = 0;         (* Change the component state *)
  OMX_CommandFlush                         = 1;         (* Flush the data queue(s) of a component *)
  OMX_CommandPortDisable                   = 2;         (* Disable a port on a component. *)
  OMX_CommandPortEnable                    = 3;         (* Enable a port on a component. *)
  OMX_CommandMarkBuffer                    = 4;         (* Mark a component/buffer for observation *)
  OMX_CommandKhronosExtensions             = $6F000000; (* Reserved region for introducing Khronos Standard Extensions *)
  OMX_CommandVendorStartUnused             = $7F000000; (* Reserved region for introducing Vendor Extensions *)
  OMX_CommandMax                           = $7FFFFFFF;

type
  OMX_STATETYPE                            = Longword;
  POMX_STATETYPE                           = ^OMX_STATETYPE;

const
  OMX_StateInvalid                         = 0;         (* component has detected that it's internal data
                                                           structures are corrupted to the point that
                                                           it cannot determine it's state properly *)
  OMX_StateLoaded                          = 1;         (* component has been loaded but has not completed
                                                           initialization.  The OMX_SetParameter macro
                                                           and the OMX_GetParameter macro are the only
                                                           valid macros allowed to be sent to the
                                                           component in this state. *)
  OMX_StateIdle                            = 2;         (* component initialization has been completed
                                                           successfully and the component is ready to
                                                           to start. *)
  OMX_StateExecuting                       = 3;         (* component has accepted the start command and
                                                           is processing data (if data is available) *)
  OMX_StatePause                           = 4;         (* component has received pause command *)
  OMX_StateWaitForResources                = 5;         (* component is waiting for resources, either after
                                                           preemption or before it gets the resources requested.
                                                           See specification for complete details. *)
  OMX_StateKhronosExtensions               = $6F000000; (* Reserved region for introducing Khronos Standard Extensions *)
  OMX_StateVendorStartUnused               = $7F000000; (* Reserved region for introducing Vendor Extensions *)
  OMX_StateMax                             = $7FFFFFFF;

type
  TGetComponentVersion = function (hComponent  : OMX_HANDLETYPE;
                                   pComponentName : OMX_STRING;
                                   pComponentVersion : POMX_VERSIONTYPE;
                                   pSpecVersion : POMX_VERSIONTYPE;
                                   pComponentUUID : POMX_UUIDTYPE) : OMX_ERRORTYPE; cdecl;

  TSendCommand = function (hComponent : OMX_HANDLETYPE;
                           Cmd : OMX_COMMANDTYPE;
                           nParam1 : OMX_U32;
                           pCmdData : OMX_PTR) : OMX_ERRORTYPE; cdecl;

  TGetParameter = function (hComponent : OMX_HANDLETYPE;
                            nParamIndex : OMX_INDEXTYPE;
                            pComponentParameterStructure : OMX_PTR) : OMX_ERRORTYPE; cdecl;

  TSetParameter = function (hComponent : OMX_HANDLETYPE;
                            nIndex : OMX_INDEXTYPE;
                            pComponentParameterStructure : OMX_PTR) : OMX_ERRORTYPE; cdecl;

  TGetConfig = function (hComponent : OMX_HANDLETYPE;
                         nIndex : OMX_INDEXTYPE;
                         pComponentConfigStructure : OMX_PTR) : OMX_ERRORTYPE; cdecl;

  TSetConfig = function (hComponent : OMX_HANDLETYPE;
                         nIndex : OMX_INDEXTYPE;
                         pComponentConfigStructure : OMX_PTR) : OMX_ERRORTYPE; cdecl;

  TGetExtensionIndex = function (hComponent : OMX_HANDLETYPE;
                                 cParameterName : OMX_STRING;
                                 pIndexType : POMX_INDEXTYPE) : OMX_ERRORTYPE; cdecl;

  TGetState = function (hComponent : OMX_HANDLETYPE;
                        pState : POMX_STATETYPE) : OMX_ERRORTYPE; cdecl;

  TComponentTunnelRequest = function (hComp : OMX_HANDLETYPE;
                                      nPort : OMX_U32;
                                      hTunneledComp : OMX_HANDLETYPE;
                                      nTunneledPort : OMX_U32;
                                      pTunnelSetup : POMX_TUNNELSETUPTYPE) : OMX_ERRORTYPE; cdecl;

  TUseBuffer = function (hComponent : OMX_HANDLETYPE;
                         ppBufferHdr : PPOMX_BUFFERHEADERTYPE;
                         nPortIndex : OMX_U32;
                         pAppPrivate : OMX_PTR;
                         nSizeBytes : OMX_U32;
                         pBuffer : POMX_U8) : OMX_ERRORTYPE; cdecl;

  TAllocateBuffer = function (hComponent : OMX_HANDLETYPE;
                              ppBuffer : PPOMX_BUFFERHEADERTYPE;
                              nPortIndex : OMX_U32;
                              pAppPrivate : OMX_PTR;
                              nSizeBytes : OMX_U32) : OMX_ERRORTYPE; cdecl;

  TFreeBuffer = function (hComponent : OMX_HANDLETYPE;
                          nPortIndex : OMX_U32;
                          pBuffer : POMX_BUFFERHEADERTYPE) : OMX_ERRORTYPE; cdecl;

  TEmptyThisBuffer = function (hComponent : OMX_HANDLETYPE;
                               pBuffer : POMX_BUFFERHEADERTYPE) : OMX_ERRORTYPE; cdecl;

  TFillThisBuffer = function (hComponent : OMX_HANDLETYPE;
                              pBuffer : POMX_BUFFERHEADERTYPE) : OMX_ERRORTYPE; cdecl;

  TSetCallbacks = function (hComponent : OMX_HANDLETYPE;
                                         pCallbacks : POMX_CALLBACKTYPE;
                                         pAppData : OMX_PTR) : OMX_ERRORTYPE; cdecl;

  TComponentDeInit = function (hComponent : OMX_HANDLETYPE) : OMX_ERRORTYPE; cdecl;

  TUseEGLImage = function (hComponent : OMX_HANDLETYPE;
                           ppBufferHdr : PPOMX_BUFFERHEADERTYPE;
                           nPortIndex : OMX_U32;
                           pAppPrivate : OMX_PTR;
                           eglImage : Pointer) : OMX_ERRORTYPE; cdecl;

  TComponentRoleEnum = function (hComponent : OMX_HANDLETYPE;
                    		         cRole : POMX_U8;
                                 nIndex : OMX_U32) : OMX_ERRORTYPE; cdecl;

  OMX_COMPONENTTYPE = record
    nSize : OMX_U32;                         (* The size of this structure, in bytes.  It is the responsibility
                                                of the allocator of this structure to fill in this value.  Since
                                                this structure is allocated by the GetHandle function, this
                                                function will fill in this value. *)
    nVersion : OMX_VERSIONTYPE;              (* nVersion is the version of the OMX specification that the structure
                                                is built against.  It is the responsibility of the creator of this
                                                structure to initialize this value and every user of this structure
                                                should verify that it knows how to use the exact version of
                                                this structure found herein. *)
    pComponentPrivate : OMX_PTR;             (* pComponentPrivate is a pointer to the component private data area.
                                                This member is allocated and initialized by the component when the
                                                component is first loaded.  The application should not access this
                                                data area. *)
    pApplicationPrivate : OMX_PTR;           (* pApplicationPrivate is a pointer that is a parameter to the
                                                OMX_GetHandle method, and contains an application private value
                                                provided by the IL client.  This application private data is
                                                returned to the IL Client by OMX in all callbacks *)
    GetComponentVersion : TGetComponentVersion; (* refer to OMX_GetComponentVersion in OMX_core.h or the OMX IL
                                                   specification for details on the GetComponentVersion method. *)
    SendCommand : TSendCommand;              (* refer to OMX_SendCommand in OMX_core.h or the OMX IL
                                                specification for details on the SendCommand method. *)
    GetParameter : TGetParameter;            (* refer to OMX_GetParameter in OMX_core.h or the OMX IL
                                                specification for details on the GetParameter method. *)
    SetParameter : TSetParameter;            (* refer to OMX_SetParameter in OMX_core.h or the OMX IL
                                                specification for details on the SetParameter method. *)
    GetConfig : TGetConfig;                  (* refer to OMX_GetConfig in OMX_core.h or the OMX IL
                                                specification for details on the GetConfig method. *)
    SetConfig : TSetConfig;                  (* refer to OMX_SetConfig in OMX_core.h or the OMX IL
                                                specification for details on the SetConfig method.*)
    GetExtensionIndex : TGetExtensionIndex;  (* refer to OMX_GetExtensionIndex in OMX_core.h or the OMX IL
                                                specification for details on the GetExtensionIndex method. *)
    GetState : TGetState;                    (* refer to OMX_GetState in OMX_core.h or the OMX IL
                                                specification for details on the GetState method. *)
    ComponentTunnelRequest : TComponentTunnelRequest; (* The ComponentTunnelRequest method will interact with another OMX
                                                         component to determine if tunneling is possible and to setup the
                                                         tunneling.  The return codes for this method can be used to
                                                         determine if tunneling is not possible, or if tunneling is not
                                                         supported.

        Base profile components (i.e. non-interop) do not support this
        method and should return OMX_ErrorNotImplemented

        The interop profile component MUST support tunneling to another
        interop profile component with a compatible port parameters.
        A component may also support proprietary communication.

        If proprietary communication is supported the negotiation of
        proprietary communication is done outside of OMX in a vendor
        specific way. It is only required that the proper result be
        returned and the details of how the setup is done is left
        to the component implementation.

        When this method is invoked when nPort in an output port, the
        component will:
        1.  Populate the pTunnelSetup structure with the output port's
            requirements and constraints for the tunnel.

        When this method is invoked when nPort in an input port, the
        component will:
        1.  Query the necessary parameters from the output port to
            determine if the ports are compatible for tunneling
        2.  If the ports are compatible, the component should store
            the tunnel step provided by the output port
        3.  Determine which port (either input or output) is the buffer
            supplier, and call OMX_SetParameter on the output port to
            indicate this selection.

        The component will return from this call within 5 msec.

        @param [in] hComp
            Handle of the component to be accessed.  This is the component
            handle returned by the call to the OMX_GetHandle method.
        @param [in] nPort
            nPort is used to select the port on the component to be used
            for tunneling.
        @param [in] hTunneledComp
            Handle of the component to tunnel with.  This is the component
            handle returned by the call to the OMX_GetHandle method.  When
            this parameter is 0x0 the component should setup the port for
            communication with the application / IL Client.
        @param [in] nPortOutput
            nPortOutput is used indicate the port the component should
            tunnel with.
        @param [in] pTunnelSetup
            Pointer to the tunnel setup structure.  When nPort is an output port
            the component should populate the fields of this structure.  When
            When nPort is an input port the component should review the setup
            provided by the component with the output port.
        @return OMX_ERRORTYPE
            If the command successfully executes, the return code will be
            OMX_ErrorNone.  Otherwise the appropriate OMX error will be returned.
        @ingroup tun  *)
    UseBuffer : TUseBuffer;                  (* refer to OMX_UseBuffer in OMX_core.h or the OMX IL
                                                specification for details on the UseBuffer method.
                                                @ingroup buf *)
    AllocateBuffer : TAllocateBuffer;        (* refer to OMX_AllocateBuffer in OMX_core.h or the OMX IL
                                                specification for details on the AllocateBuffer method.
                                                @ingroup buf *)
    FreeBuffer : TFreeBuffer;                (* refer to OMX_FreeBuffer in OMX_core.h or the OMX IL
                                                specification for details on the FreeBuffer method.
                                                @ingroup buf *)
    EmptyThisBuffer : TEmptyThisBuffer;      (* refer to OMX_EmptyThisBuffer in OMX_core.h or the OMX IL
                                                specification for details on the EmptyThisBuffer method.
                                                @ingroup buf  *)
    FillThisBuffer : TFillThisBuffer;        (* refer to OMX_FillThisBuffer in OMX_core.h or the OMX IL
                                                specification for details on the FillThisBuffer method.
                                                @ingroup buf *)
    SetCallbacks : TSetCallbacks;            (* The SetCallbacks method is used by the core to specify the callback
                                                structure from the application to the component.  This is a blocking
                                                call.  The component will return from this call within 5 msec.

        @param [in] hComponent
            Handle of the component to be accessed.  This is the component
            handle returned by the call to the GetHandle function.
        @param [in] pCallbacks
            pointer to an OMX_CALLBACKTYPE structure used to provide the
            callback information to the component
        @param [in] pAppData
            pointer to an application defined value.  It is anticipated that
            the application will pass a pointer to a data structure or a "this
            pointer" in this area to allow the callback (in the application)
            to determine the context of the call
        @return OMX_ERRORTYPE
            If the command successfully executes, the return code will be
            OMX_ErrorNone.  Otherwise the appropriate OMX error will be returned. *)
    ComponentDeInit : TComponentDeInit;        (* ComponentDeInit method is used to deinitialize the component
                                                  providing a means to free any resources allocated at component
                                                  initialization.  NOTE:  After this call the component handle is
                                                  not valid for further use.
        @param [in] hComponent
            Handle of the component to be accessed.  This is the component
            handle returned by the call to the GetHandle function.
        @return OMX_ERRORTYPE
            If the command successfully executes, the return code will be
            OMX_ErrorNone.  Otherwise the appropriate OMX error will be returned.   *)
    UseEGLImage : TUseEGLImage;
    ComponentRoleEnum : TComponentRoleEnum;
  end;
  POMX_COMPONENTTYPE = ^OMX_COMPONENTTYPE;


  OMX_AUDIO_CODINGTYPE = LongWord;

  OMX_AUDIO_PORTDEFINITIONTYPE = record
    cMIMEType : OMX_STRING;                (* MIME type of data for the port *)
    pNativeRender : OMX_NATIVE_DEVICETYPE; (* platform specific reference
                                              for an output device,
                                              otherwise this field is 0 *)
    bFlagErrorConcealment : OMX_BOOL;      (* Turns on error concealment if it is
                                              supported by the OMX component *)
    eEncoding : OMX_AUDIO_CODINGTYPE;      (* Type of data expected for this
                                              port (e.g. PCM, AMR, MP3, etc) *)
  end;
  POMX_AUDIO_PORTDEFINITIONTYPE = ^OMX_AUDIO_PORTDEFINITIONTYPE;

  OMX_VIDEO_CODINGTYPE = LongWord;
  POMX_VIDEO_CODINGTYPE = ^OMX_VIDEO_CODINGTYPE;

const
  OMX_VIDEO_CodingUnused                   = 0;          (* Value when coding is N/A *)
  OMX_VIDEO_CodingAutoDetect               = 1;          (* Autodetection of coding type *)
  OMX_VIDEO_CodingMPEG2                    = 2;          (* AKA: H.262 *)
  OMX_VIDEO_CodingH263                     = 3;          (* H.263 *)
  OMX_VIDEO_CodingMPEG4                    = 4;          (* MPEG-4 *)
  OMX_VIDEO_CodingWMV                      = 5;          (* all versions of Windows Media Video *)
  OMX_VIDEO_CodingRV                       = 6;          (* all versions of Real Video *)
  OMX_VIDEO_CodingAVC                      = 7;          (* H.264/AVC *)
  OMX_VIDEO_CodingMJPEG                    = 8;          (* Motion JPEG *)
  OMX_VIDEO_CodingKhronosExtensions        = $6F000000;  (* Reserved region for introducing Khronos Standard Extensions *)
  OMX_VIDEO_CodingVendorStartUnused        = $7F000000;  (* Reserved region for introducing Vendor Extensions *)

type
  OMX_COLOR_FORMATTYPE = LongWord;

  OMX_VIDEO_PORTDEFINITIONTYPE = record
    cMIMEType : OMX_STRING;
    pNativeRender : OMX_NATIVE_DEVICETYPE;
    nFrameWidth : OMX_U32;
    nFrameHeight : OMX_U32;
    nStride : OMX_S32;
    nSliceHeight : OMX_U32;
    nBitrate : OMX_U32;
    xFramerate : OMX_U32;
    bFlagErrorConcealment : OMX_BOOL;
    eCompressionFormat : OMX_VIDEO_CODINGTYPE;
    eColorFormat : OMX_COLOR_FORMATTYPE;
    pNativeWindow : OMX_NATIVE_WINDOWTYPE;
  end;
  POMX_VIDEO_PORTDEFINITIONTYPE = ^OMX_VIDEO_PORTDEFINITIONTYPE;

  OMX_VIDEO_PARAM_PORTFORMATTYPE = record
    nSize : OMX_U32;
    nVersion : OMX_VERSIONTYPE;
    nPortIndex : OMX_U32;
    nIndex : OMX_U32;
    eCompressionFormat : OMX_VIDEO_CODINGTYPE;
    eColorFormat : OMX_COLOR_FORMATTYPE;
    xFramerate : OMX_U32;
  end;
  POMX_VIDEO_PARAM_PORTFORMATTYPE = ^OMX_VIDEO_PARAM_PORTFORMATTYPE;

  OMX_IMAGE_CODINGTYPE = LongWord;
  POMX_IMAGE_CODINGTYPE = ^OMX_IMAGE_CODINGTYPE;

const
  OMX_IMAGE_CodingUnused                   = $0;         (* Value when format is N/A *)
  OMX_IMAGE_CodingAutoDetect               = $1;         (* Auto detection of image format *)
  OMX_IMAGE_CodingJPEG                     = $2;         (* JPEG/JFIF image format *)
  OMX_IMAGE_CodingJPEG2K                   = $3;         (* JPEG 2000 image format *)
  OMX_IMAGE_CodingEXIF                     = $4;         (* EXIF image format *)
  OMX_IMAGE_CodingTIFF                     = $5;         (* TIFF image format *)
  OMX_IMAGE_CodingGIF                      = $6;         (* Graphics image format *)
  OMX_IMAGE_CodingPNG                      = $7;         (* PNG image format *)
  OMX_IMAGE_CodingLZW                      = $8;         (* LZW image format *)
  OMX_IMAGE_CodingBMP                      = $9;         (* Windows Bitmap format *)
  OMX_IMAGE_CodingKhronosExtensions        = $6F000000;  (* Reserved region for introducing Khronos Standard Extensions *)
  OMX_IMAGE_CodingVendorStartUnused        = $7F000000;  (* Reserved region for introducing Vendor Extensions *)
  OMX_IMAGE_CodingTGA                      = $7F000001;
  OMX_IMAGE_CodingPPM                      = $7F000002;
  OMX_IMAGE_CodingMax                      = $7FFFFFFF;

type
  OMX_IMAGE_PORTDEFINITIONTYPE = record
    cMIMEType : OMX_STRING;
    pNativeRender : OMX_NATIVE_DEVICETYPE;
    nFrameWidth : OMX_U32;
    nFrameHeight : OMX_U32;
    nStride : OMX_S32;
    nSliceHeight : OMX_U32;
    bFlagErrorConcealment : OMX_BOOL;
    eCompressionFormat : OMX_IMAGE_CODINGTYPE;
    eColorFormat : OMX_COLOR_FORMATTYPE;
    pNativeWindow : OMX_NATIVE_WINDOWTYPE;
  end;
  POMX_IMAGE_PORTDEFINITIONTYPE = ^OMX_IMAGE_PORTDEFINITIONTYPE;

  OMX_OTHER_FORMATTYPE = LongWord;

const

  OMX_OTHER_FormatTime                     = 0;          (* Transmission of various timestamps, elapsed time,
                                                            time deltas, etc *)
  OMX_OTHER_FormatPower                    = 1;          (* Perhaps used for enabling/disabling power
                                                            management, setting clocks? *)
  OMX_OTHER_FormatStats                    = 2;          (* Could be things such as frame rate, frames
                                                            dropped, etc *)
  OMX_OTHER_FormatBinary                   = 3;          (* Arbitrary binary data *)
  OMX_OTHER_FormatVendorReserved           = 1000;       (* Starting value for vendor specific
                                                            formats *)
  OMX_OTHER_FormatKhronosExtensions        = $6F000000;  (* Reserved region for introducing Khronos Standard Extensions *)
  OMX_OTHER_FormatVendorStartUnused        = $7F000000;  (* Reserved region for introducing Vendor Extensions *)
  OMX_OTHER_FormatText                     = $7F000001;
  OMX_OTHER_FormatTextSKM2                 = $7F000002;
  OMX_OTHER_FormatText3GP5                 = $7F000003;
  OMX_OTHER_FormatMax                      = $7FFFFFFF;

type
  OMX_TIME_CLOCKSTATE = LongWord;

const
  OMX_TIME_ClockStateRunning               = 0; (* Clock running. *)
  OMX_TIME_ClockStateWaitingForStartTime   = 1; (* Clock waiting until the
                                                   prescribed clients emit their
                                                   start time. *)
  OMX_TIME_ClockStateStopped = 2;               (* Clock stopped. *)
  OMX_TIME_ClockStateKhronosExtensions     = $6F000000; (* Reserved region for introducing Khronos Standard Extensions *)
  OMX_TIME_ClockStateVendorStartUnused     = $7F000000; (* Reserved region for introducing Vendor Extensions *)
  OMX_TIME_ClockStateMax                   = $7FFFFFFF;

type
  OMX_TIME_CONFIG_CLOCKSTATETYPE = record
    nSize : OMX_U32;                     (* size of the structure in bytes *)
    nVersion : OMX_VERSIONTYPE;          (* OMX specification version information *)
    eState : OMX_TIME_CLOCKSTATE;        (* State of the media time. *)
    nStartTime : OMX_TICKS;              (* Start time of the media time. *)
    nOffset : OMX_TICKS;                 (* Time to offset the media time by
                                            (e.g. preroll). Media time will be
                                            reported to be nOffset ticks earlier. *)
    nWaitMask : OMX_U32;                 (* Mask of OMX_CLOCKPORT values. *)
  end;

  OMX_OTHER_PORTDEFINITIONTYPE = record
    eFormat : OMX_OTHER_FORMATTYPE;  (* Type of data expected for this channel *)
  end;
  POMX_OTHER_PORTDEFINITIONTYPE = ^OMX_OTHER_PORTDEFINITIONTYPE;

  OMX_FORMAT = record
    case integer of
     1 : (audio : OMX_AUDIO_PORTDEFINITIONTYPE);
     2 : (video : OMX_VIDEO_PORTDEFINITIONTYPE);
     3 : (image : OMX_IMAGE_PORTDEFINITIONTYPE);
     4 : (other : OMX_OTHER_PORTDEFINITIONTYPE);
  end;

  OMX_PARAM_PORTDEFINITIONTYPE = record
    nSize : OMX_U32;                 (* Size of the structure in bytes *)
    nVersion : OMX_VERSIONTYPE;      (* OMX specification version information *)
    nPortIndex : OMX_U32;            (* Port number the structure applies to *)
    eDir : OMX_DIRTYPE;              (* Direction (input or output) of this port *)
    nBufferCountActual : OMX_U32;    (* The actual number of buffers allocated on this port *)
    nBufferCountMin : OMX_U32;       (* The minimum number of buffers this port requires *)
    nBufferSize : OMX_U32;           (* Size, in bytes, for buffers to be used for this channel *)
    bEnabled : OMX_BOOL;             (* Ports default to enabled and are enabled/disabled by
                                        OMX_CommandPortEnable/OMX_CommandPortDisable.
                                        When disabled a port is unpopulated. A disabled port
                                        is not populated with buffers on a transition to IDLE. *)
    bPopulated : OMX_BOOL;           (* Port is populated with all of its buffers as indicated by
                                        nBufferCountActual. A disabled port is always unpopulated.
                                        An enabled port is populated on a transition to OMX_StateIdle
                                        and unpopulated on a transition to loaded. *)
    eDomain : OMX_PORTDOMAINTYPE;    (* Domain of the port. Determines the contents of metadata below. *)
    format : OMX_FORMAT;
    bBuffersContiguous : OMX_BOOL;
    nBufferAlignment : OMX_U32;
  end;

  OMX_AUDIO_PCMMODETYPE = Longword;

const
  OMX_AUDIO_PCMModeLinear                  = 0;          (* Linear PCM encoded data *)
  OMX_AUDIO_PCMModeALaw                    = 1;          (* A law PCM encoded data (G.711) *)
  OMX_AUDIO_PCMModeMULaw                   = 2;          (* Mu law PCM encoded data (G.711)  *)
  OMX_AUDIO_PCMModeKhronosExtensions       = $6F000000;  (* Reserved region for introducing Khronos Standard Extensions *)
  OMX_AUDIO_PCMModeVendorStartUnused       = $7F000000;  (* Reserved region for introducing Vendor Extensions *)
  OMX_AUDIO_PCMModeMax                     = $7FFFFFFF;

type
  OMX_AUDIO_CHANNELTYPE = Longword;

const
  OMX_AUDIO_ChannelNone                    = $0;         (* Unused or empty *)
  OMX_AUDIO_ChannelLF                      = $1;         (* Left front *)
  OMX_AUDIO_ChannelRF                      = $2;         (* Right front *)
  OMX_AUDIO_ChannelCF                      = $3;         (* Center front *)
  OMX_AUDIO_ChannelLS                      = $4;         (* Left surround *)
  OMX_AUDIO_ChannelRS                      = $5;         (* Right surround *)
  OMX_AUDIO_ChannelLFE                     = $6;         (* Low frequency effects *)
  OMX_AUDIO_ChannelCS                      = $7;         (* Back surround *)
  OMX_AUDIO_ChannelLR                      = $8;         (* Left rear. *)
  OMX_AUDIO_ChannelRR                      = $9;         (* Right rear. *)
  OMX_AUDIO_ChannelKhronosExtensions       = $6F000000;  (* Reserved region for introducing Khronos Standard Extensions *)
  OMX_AUDIO_ChannelVendorStartUnused       = $7F000000;  (* Reserved region for introducing Vendor Extensions *)
  OMX_AUDIO_ChannelMax                     = $7FFFFFFF;

  OMX_AUDIO_MAXCHANNELS                    = 16;         (* maximum number distinct audio channels that a buffer may contain *)

type
  OMX_AUDIO_PARAM_PCMMODETYPE = record
    nSize : OMX_U32;                    (* Size of this structure, in Bytes *)
    nVersion : OMX_VERSIONTYPE;         (* OMX specification version information *)
    nPortIndex : OMX_U32;               (* port that this structure applies to *)
    nChannels : OMX_U32;                (* Number of channels (e.g. 2 for stereo) *)
    eNumData : OMX_NUMERICALDATATYPE;   (* indicates PCM data as signed or unsigned *)
    eEndian : OMX_ENDIANTYPE;           (* indicates PCM data as little or big endian *)
    bInterleaved : OMX_BOOL;            (* True for normal interleaved data; false for
                                           non-interleaved data (e.g. block data) *)
    nBitPerSample : OMX_U32;            (* Bit per sample *)
    nSamplingRate : OMX_U32;            (* Sampling rate of the source data.  Use 0 for
                                           variable or unknown sampling rate. *)
    ePCMMode : OMX_AUDIO_PCMMODETYPE;   (* PCM mode enumeration *)
    eChannelMapping : array [0 .. OMX_AUDIO_MAXCHANNELS - 1] of OMX_AUDIO_CHANNELTYPE; (* Slot i contains channel defined by eChannelMap[i] *)
  end;


const
  // OMX_INDEX_TYPE
  OMX_IndexComponentStartUnused            = $01000000;
  OMX_IndexParamPriorityMgmt               = $01000001;  (* reference: OMX_PRIORITYMGMTTYPE *)
  OMX_IndexParamAudioInit                  = $01000002;  (* reference: OMX_PORT_PARAM_TYPE *)
  OMX_IndexParamImageInit                  = $01000003;  (* reference: OMX_PORT_PARAM_TYPE *)
  OMX_IndexParamVideoInit                  = $01000004;  (* reference: OMX_PORT_PARAM_TYPE *)
  OMX_IndexParamOtherInit                  = $01000005;  (* reference: OMX_PORT_PARAM_TYPE *)
  OMX_IndexParamNumAvailableStreams        = $01000006;  (* reference: OMX_PARAM_U32TYPE *)
  OMX_IndexParamActiveStream               = $01000007;  (* reference: OMX_PARAM_U32TYPE *)
  OMX_IndexParamSuspensionPolicy           = $01000008;  (* reference: OMX_PARAM_SUSPENSIONPOLICYTYPE *)
  OMX_IndexParamComponentSuspended         = $01000009;  (* reference: OMX_PARAM_SUSPENSIONTYPE *)
  OMX_IndexConfigCapturing                 = $0100000A;  (* reference: OMX_CONFIG_BOOLEANTYPE *)
  OMX_IndexConfigCaptureMode               = $0100000B;  (* reference: OMX_CONFIG_CAPTUREMODETYPE *)
  OMX_IndexAutoPauseAfterCapture           = $0100000C;  (* reference: OMX_CONFIG_BOOLEANTYPE *)
  OMX_IndexParamContentURI                 = $0100000D;  (* reference: OMX_PARAM_CONTENTURITYPE *)
  OMX_IndexParamCustomContentPipe          = $0100000E;  (* reference: OMX_PARAM_CONTENTPIPETYPE *)
  OMX_IndexParamDisableResourceConcealment = $0100000F;  (* reference: OMX_RESOURCECONCEALMENTTYPE *)
  OMX_IndexConfigMetadataItemCount         = $01000010;  (* reference: OMX_CONFIG_METADATAITEMCOUNTTYPE *)
  OMX_IndexConfigContainerNodeCount        = $01000011;  (* reference: OMX_CONFIG_CONTAINERNODECOUNTTYPE *)
  OMX_IndexConfigMetadataItem              = $01000012;  (* reference: OMX_CONFIG_METADATAITEMTYPE *)
  OMX_IndexConfigCounterNodeID             = $01000013;  (* reference: OMX_CONFIG_CONTAINERNODEIDTYPE *)
  OMX_IndexParamMetadataFilterType         = $01000014;  (* reference: OMX_PARAM_METADATAFILTERTYPE *)
  OMX_IndexParamMetadataKeyFilter          = $01000015;  (* reference: OMX_PARAM_METADATAFILTERTYPE *)
  OMX_IndexConfigPriorityMgmt              = $01000016;  (* reference: OMX_PRIORITYMGMTTYPE *)
  OMX_IndexParamStandardComponentRole      = $01000017;  (* reference: OMX_PARAM_COMPONENTROLETYPE *)

  OMX_IndexPortStartUnused                 = $02000000;
  OMX_IndexParamPortDefinition             = $02000001;  (* reference: OMX_PARAM_PORTDEFINITIONTYPE *)
  OMX_IndexParamCompBufferSupplier         = $02000002;  (* reference: OMX_PARAM_BUFFERSUPPLIERTYPE *)

  OMX_IndexReservedStartUnused             = $03000000;

  (* Audio parameters and configurations *)
  OMX_IndexAudioStartUnused                = $04000000;
  OMX_IndexParamAudioPortFormat            = $04000001;  (* reference: OMX_AUDIO_PARAM_PORTFORMATTYPE *)
  OMX_IndexParamAudioPcm                   = $04000002;  (* reference: OMX_AUDIO_PARAM_PCMMODETYPE *)
  OMX_IndexParamAudioAac                   = $04000003;  (* reference: OMX_AUDIO_PARAM_AACPROFILETYPE *)
  OMX_IndexParamAudioRa                    = $04000004;  (* reference: OMX_AUDIO_PARAM_RATYPE *)
  OMX_IndexParamAudioMp3                   = $04000005;  (* reference: OMX_AUDIO_PARAM_MP3TYPE *)
  OMX_IndexParamAudioAdpcm                 = $04000006;  (* reference: OMX_AUDIO_PARAM_ADPCMTYPE *)
  OMX_IndexParamAudioG723                  = $04000007;  (* reference: OMX_AUDIO_PARAM_G723TYPE *)
  OMX_IndexParamAudioG729                  = $04000008;  (* reference: OMX_AUDIO_PARAM_G729TYPE *)
  OMX_IndexParamAudioAmr                   = $04000009;  (* reference: OMX_AUDIO_PARAM_AMRTYPE *)
  OMX_IndexParamAudioWma                   = $0400000A;  (* reference: OMX_AUDIO_PARAM_WMATYPE *)
  OMX_IndexParamAudioSbc                   = $0400000B;  (* reference: OMX_AUDIO_PARAM_SBCTYPE *)
  OMX_IndexParamAudioMidi                  = $0400000C;  (* reference: OMX_AUDIO_PARAM_MIDITYPE *)
  OMX_IndexParamAudioGsm_FR                = $0400000D;  (* reference: OMX_AUDIO_PARAM_GSMFRTYPE *)
  OMX_IndexParamAudioMidiLoadUserSound     = $0400000E;  (* reference: OMX_AUDIO_PARAM_MIDILOADUSERSOUNDTYPE *)
  OMX_IndexParamAudioG726                  = $0400000F;  (* reference: OMX_AUDIO_PARAM_G726TYPE *)
  OMX_IndexParamAudioGsm_EFR               = $04000010;  (* reference: OMX_AUDIO_PARAM_GSMEFRTYPE *)
  OMX_IndexParamAudioGsm_HR                = $04000011;  (* reference: OMX_AUDIO_PARAM_GSMHRTYPE *)
  OMX_IndexParamAudioPdc_FR                = $04000012;  (* reference: OMX_AUDIO_PARAM_PDCFRTYPE *)
  OMX_IndexParamAudioPdc_EFR               = $04000013;  (* reference: OMX_AUDIO_PARAM_PDCEFRTYPE *)
  OMX_IndexParamAudioPdc_HR                = $04000014;  (* reference: OMX_AUDIO_PARAM_PDCHRTYPE *)
  OMX_IndexParamAudioTdma_FR               = $04000015;  (* reference: OMX_AUDIO_PARAM_TDMAFRTYPE *)
  OMX_IndexParamAudioTdma_EFR              = $04000016;  (* reference: OMX_AUDIO_PARAM_TDMAEFRTYPE *)
  OMX_IndexParamAudioQcelp8                = $04000017;  (* reference: OMX_AUDIO_PARAM_QCELP8TYPE *)
  OMX_IndexParamAudioQcelp13               = $04000018;  (* reference: OMX_AUDIO_PARAM_QCELP13TYPE *)
  OMX_IndexParamAudioEvrc                  = $04000019;  (* reference: OMX_AUDIO_PARAM_EVRCTYPE *)
  OMX_IndexParamAudioSmv                   = $0400001A;  (* reference: OMX_AUDIO_PARAM_SMVTYPE *)
  OMX_IndexParamAudioVorbis                = $0400001B;  (* reference: OMX_AUDIO_PARAM_VORBISTYPE *)

  OMX_IndexConfigAudioMidiImmediateEvent   = $0400001C;  (* reference: OMX_AUDIO_CONFIG_MIDIIMMEDIATEEVENTTYPE *)
  OMX_IndexConfigAudioMidiControl          = $0400001D;  (* reference: OMX_AUDIO_CONFIG_MIDICONTROLTYPE *)
  OMX_IndexConfigAudioMidiSoundBankProgram = $0400001E;  (* reference: OMX_AUDIO_CONFIG_MIDISOUNDBANKPROGRAMTYPE *)
  OMX_IndexConfigAudioMidiStatus           = $0400001F;  (* reference: OMX_AUDIO_CONFIG_MIDISTATUSTYPE *)
  OMX_IndexConfigAudioMidiMetaEvent        = $04000020;  (* reference: OMX_AUDIO_CONFIG_MIDIMETAEVENTTYPE *)
  OMX_IndexConfigAudioMidiMetaEventData    = $04000021;  (* reference: OMX_AUDIO_CONFIG_MIDIMETAEVENTDATATYPE *)
  OMX_IndexConfigAudioVolume               = $04000022;  (* reference: OMX_AUDIO_CONFIG_VOLUMETYPE *)
  OMX_IndexConfigAudioBalance              = $04000023;  (* reference: OMX_AUDIO_CONFIG_BALANCETYPE *)
  OMX_IndexConfigAudioChannelMute          = $04000024;  (* reference: OMX_AUDIO_CONFIG_CHANNELMUTETYPE *)
  OMX_IndexConfigAudioMute                 = $04000025;  (* reference: OMX_AUDIO_CONFIG_MUTETYPE *)
  OMX_IndexConfigAudioLoudness             = $04000026;  (* reference: OMX_AUDIO_CONFIG_LOUDNESSTYPE *)
  OMX_IndexConfigAudioEchoCancelation      = $04000027;  (* reference: OMX_AUDIO_CONFIG_ECHOCANCELATIONTYPE *)
  OMX_IndexConfigAudioNoiseReduction       = $04000028;  (* reference: OMX_AUDIO_CONFIG_NOISEREDUCTIONTYPE *)
  OMX_IndexConfigAudioBass                 = $04000029;  (* reference: OMX_AUDIO_CONFIG_BASSTYPE *)
  OMX_IndexConfigAudioTreble               = $0400002A;  (* reference: OMX_AUDIO_CONFIG_TREBLETYPE *)
  OMX_IndexConfigAudioStereoWidening       = $0400002B;  (* reference: OMX_AUDIO_CONFIG_STEREOWIDENINGTYPE *)
  OMX_IndexConfigAudioChorus               = $0400002C;  (* reference: OMX_AUDIO_CONFIG_CHORUSTYPE *)
  OMX_IndexConfigAudioEqualizer            = $0400002D;  (* reference: OMX_AUDIO_CONFIG_EQUALIZERTYPE *)
  OMX_IndexConfigAudioReverberation        = $0400002E;  (* reference: OMX_AUDIO_CONFIG_REVERBERATIONTYPE *)
  OMX_IndexConfigAudioChannelVolume        = $0400002F;  (* reference: OMX_AUDIO_CONFIG_CHANNELVOLUMETYPE *)

  (* Image specific parameters and configurations *)
  OMX_IndexImageStartUnused                = $05000000;
  OMX_IndexParamImagePortFormat            = $05000001;  (* reference: OMX_IMAGE_PARAM_PORTFORMATTYPE *)
  OMX_IndexParamFlashControl               = $05000002;  (* reference: OMX_IMAGE_PARAM_FLASHCONTROLTYPE *)
  OMX_IndexConfigFocusControl              = $05000003;  (* reference: OMX_IMAGE_CONFIG_FOCUSCONTROLTYPE *)
  OMX_IndexParamQFactor                    = $05000004;  (* reference: OMX_IMAGE_PARAM_QFACTORTYPE *)
  OMX_IndexParamQuantizationTable          = $05000005;  (* reference: OMX_IMAGE_PARAM_QUANTIZATIONTABLETYPE *)
  OMX_IndexParamHuffmanTable               = $05000006;  (* reference: OMX_IMAGE_PARAM_HUFFMANTTABLETYPE *)
  OMX_IndexConfigFlashControl              = $05000007;  (* reference: OMX_IMAGE_PARAM_FLASHCONTROLTYPE *)

  (* Video specific parameters and configurations *)
  OMX_IndexVideoStartUnused                = $06000000;
  OMX_IndexParamVideoPortFormat            = $06000001;  (* reference: OMX_VIDEO_PARAM_PORTFORMATTYPE *)
  OMX_IndexParamVideoQuantization          = $06000002;  (* reference: OMX_VIDEO_PARAM_QUANTIZATIONTYPE *)
  OMX_IndexParamVideoFastUpdate            = $06000003;  (* reference: OMX_VIDEO_PARAM_VIDEOFASTUPDATETYPE *)
  OMX_IndexParamVideoBitrate               = $06000004;  (* reference: OMX_VIDEO_PARAM_BITRATETYPE *)
  OMX_IndexParamVideoMotionVector          = $06000005;  (* reference: OMX_VIDEO_PARAM_MOTIONVECTORTYPE *)
  OMX_IndexParamVideoIntraRefresh          = $06000006;  (* reference: OMX_VIDEO_PARAM_INTRAREFRESHTYPE *)
  OMX_IndexParamVideoErrorCorrection       = $06000007;  (* reference: OMX_VIDEO_PARAM_ERRORCORRECTIONTYPE *)
  OMX_IndexParamVideoVBSMC                 = $06000008;  (* reference: OMX_VIDEO_PARAM_VBSMCTYPE *)
  OMX_IndexParamVideoMpeg2                 = $06000009;  (* reference: OMX_VIDEO_PARAM_MPEG2TYPE *)
  OMX_IndexParamVideoMpeg4                 = $0600000A;  (* reference: OMX_VIDEO_PARAM_MPEG4TYPE *)
  OMX_IndexParamVideoWmv                   = $0600000B;  (* reference: OMX_VIDEO_PARAM_WMVTYPE *)
  OMX_IndexParamVideoRv                    = $0600000C;  (* reference: OMX_VIDEO_PARAM_RVTYPE *)
  OMX_IndexParamVideoAvc                   = $0600000D;  (* reference: OMX_VIDEO_PARAM_AVCTYPE *)
  OMX_IndexParamVideoH263                  = $0600000E;  (* reference: OMX_VIDEO_PARAM_H263TYPE *)
  OMX_IndexParamVideoProfileLevelQuerySupported = $0600000F; (* reference: OMX_VIDEO_PARAM_PROFILELEVELTYPE *)
  OMX_IndexParamVideoProfileLevelCurrent   = $06000010;  (* reference: OMX_VIDEO_PARAM_PROFILELEVELTYPE *)
  OMX_IndexConfigVideoBitrate              = $06000011;  (* reference: OMX_VIDEO_CONFIG_BITRATETYPE *)
  OMX_IndexConfigVideoFramerate            = $06000012;  (* reference: OMX_CONFIG_FRAMERATETYPE *)
  OMX_IndexConfigVideoIntraVOPRefresh      = $06000013;  (* reference: OMX_CONFIG_INTRAREFRESHVOPTYPE *)
  OMX_IndexConfigVideoIntraMBRefresh       = $06000014;  (* reference: OMX_CONFIG_MACROBLOCKERRORMAPTYPE *)
  OMX_IndexConfigVideoMBErrorReporting     = $06000015;  (* reference: OMX_CONFIG_MBERRORREPORTINGTYPE *)
  OMX_IndexParamVideoMacroblocksPerFrame   = $06000016;  (* reference: OMX_PARAM_MACROBLOCKSTYPE *)
  OMX_IndexConfigVideoMacroBlockErrorMap   = $06000017;  (* reference: OMX_CONFIG_MACROBLOCKERRORMAPTYPE *)
  OMX_IndexParamVideoSliceFMO              = $06000018;  (* reference: OMX_VIDEO_PARAM_AVCSLICEFMO *)
  OMX_IndexConfigVideoAVCIntraPeriod       = $06000019;  (* reference: OMX_VIDEO_CONFIG_AVCINTRAPERIOD *)
  OMX_IndexConfigVideoNalSize              = $0600001A;  (* reference: OMX_VIDEO_CONFIG_NALSIZE *)

  (* Image & Video common Configurations *)
  OMX_IndexCommonStartUnused               = $07000000;
  OMX_IndexParamCommonDeblocking           = $07000001;  (* reference: OMX_PARAM_DEBLOCKINGTYPE *)
  OMX_IndexParamCommonSensorMode           = $07000002;  (* reference: OMX_PARAM_SENSORMODETYPE *)
  OMX_IndexParamCommonInterleave           = $07000003;  (* reference: OMX_PARAM_INTERLEAVETYPE *)
  OMX_IndexConfigCommonColorFormatConversion = $07000004; (* reference: OMX_CONFIG_COLORCONVERSIONTYPE *)
  OMX_IndexConfigCommonScale               = $07000005;  (* reference: OMX_CONFIG_SCALEFACTORTYPE *)
  OMX_IndexConfigCommonImageFilter         = $07000006;  (* reference: OMX_CONFIG_IMAGEFILTERTYPE *)
  OMX_IndexConfigCommonColorEnhancement    = $07000007;  (* reference: OMX_CONFIG_COLORENHANCEMENTTYPE *)
  OMX_IndexConfigCommonColorKey            = $07000008;  (* reference: OMX_CONFIG_COLORKEYTYPE *)
  OMX_IndexConfigCommonColorBlend          = $07000009;  (* reference: OMX_CONFIG_COLORBLENDTYPE *)
  OMX_IndexConfigCommonFrameStabilisation  = $0700000A;  (* reference: OMX_CONFIG_FRAMESTABTYPE *)
  OMX_IndexConfigCommonRotate              = $0700000B;  (* reference: OMX_CONFIG_ROTATIONTYPE *)
  OMX_IndexConfigCommonMirror              = $0700000C;  (* reference: OMX_CONFIG_MIRRORTYPE *)
  OMX_IndexConfigCommonOutputPosition      = $0700000D;  (* reference: OMX_CONFIG_POINTTYPE *)
  OMX_IndexConfigCommonInputCrop           = $0700000E;  (* reference: OMX_CONFIG_RECTTYPE *)
  OMX_IndexConfigCommonOutputCrop          = $0700000F;  (* reference: OMX_CONFIG_RECTTYPE *)
  OMX_IndexConfigCommonDigitalZoom         = $07000010;  (* reference: OMX_CONFIG_SCALEFACTORTYPE *)
  OMX_IndexConfigCommonOpticalZoom         = $07000011;  (* reference: OMX_CONFIG_SCALEFACTORTYPE*)
  OMX_IndexConfigCommonWhiteBalance        = $07000012;  (* reference: OMX_CONFIG_WHITEBALCONTROLTYPE *)
  OMX_IndexConfigCommonExposure            = $07000013;  (* reference: OMX_CONFIG_EXPOSURECONTROLTYPE *)
  OMX_IndexConfigCommonContrast            = $07000014;  (* reference: OMX_CONFIG_CONTRASTTYPE *)
  OMX_IndexConfigCommonBrightness          = $07000015;  (* reference: OMX_CONFIG_BRIGHTNESSTYPE *)
  OMX_IndexConfigCommonBacklight           = $07000016;  (* reference: OMX_CONFIG_BACKLIGHTTYPE *)
  OMX_IndexConfigCommonGamma               = $07000017;  (* reference: OMX_CONFIG_GAMMATYPE *)
  OMX_IndexConfigCommonSaturation          = $07000018;  (* reference: OMX_CONFIG_SATURATIONTYPE *)
  OMX_IndexConfigCommonLightness           = $07000019;  (* reference: OMX_CONFIG_LIGHTNESSTYPE *)
  OMX_IndexConfigCommonExclusionRect       = $0700001A;  (* reference: OMX_CONFIG_RECTTYPE *)
  OMX_IndexConfigCommonDithering           = $0700001B;  (* reference: OMX_CONFIG_DITHERTYPE *)
  OMX_IndexConfigCommonPlaneBlend          = $0700001C;  (* reference: OMX_CONFIG_PLANEBLENDTYPE *)
  OMX_IndexConfigCommonExposureValue       = $0700001D;  (* reference: OMX_CONFIG_EXPOSUREVALUETYPE *)
  OMX_IndexConfigCommonOutputSize          = $0700001E;  (* reference: OMX_FRAMESIZETYPE *)
  OMX_IndexParamCommonExtraQuantData       = $0700001F;  (* reference: OMX_OTHER_EXTRADATATYPE *)
  OMX_IndexConfigCommonFocusRegion         = $07000020;  (* reference: OMX_CONFIG_FOCUSREGIONTYPE *)
  OMX_IndexConfigCommonFocusStatus         = $07000021;  (* reference: OMX_PARAM_FOCUSSTATUSTYPE *)
  OMX_IndexConfigCommonTransitionEffect    = $07000022;  (* reference: OMX_CONFIG_TRANSITIONEFFECTTYPE *)

  (* Reserved Configuration range *)
  OMX_IndexOtherStartUnused                = $08000000;
  OMX_IndexParamOtherPortFormat            = $08000001;  (* reference: OMX_OTHER_PARAM_PORTFORMATTYPE *)
  OMX_IndexConfigOtherPower                = $08000002;  (* reference: OMX_OTHER_CONFIG_POWERTYPE *)
  OMX_IndexConfigOtherStats                = $08000003;  (* reference: OMX_OTHER_CONFIG_STATSTYPE *)

  (* Reserved Time range *)
  OMX_IndexTimeStartUnused                 = $09000000;
  OMX_IndexConfigTimeScale                 = $09000001;  (* reference: OMX_TIME_CONFIG_CLOCKSTATETYPE *)
  OMX_IndexConfigTimeClockState            = $09000002;  (* reference: OMX_TIME_CONFIG_CLOCKSTATETYPE *)
  OMX_IndexConfigTimeActiveRefClock        = $09000003;  (* reference: OMX_TIME_CONFIG_ACTIVEREFCLOCKTYPE *)
  OMX_IndexConfigTimeCurrentMediaTime      = $09000004;  (* reference: OMX_TIME_CONFIG_TIMESTAMPTYPE (read only) *)
  OMX_IndexConfigTimeCurrentWallTime       = $09000005;  (* reference: OMX_TIME_CONFIG_TIMESTAMPTYPE (read only) *)
  OMX_IndexConfigTimeCurrentAudioReference = $09000006;  (* reference: OMX_TIME_CONFIG_TIMESTAMPTYPE (write only) *)
  OMX_IndexConfigTimeCurrentVideoReference = $09000007;  (* reference: OMX_TIME_CONFIG_TIMESTAMPTYPE (write only) *)
  OMX_IndexConfigTimeMediaTimeRequest      = $09000008;  (* reference: OMX_TIME_CONFIG_MEDIATIMEREQUESTTYPE (write only) *)
  OMX_IndexConfigTimeClientStartTime       = $09000009;  (* reference: OMX_TIME_CONFIG_TIMESTAMPTYPE (write only) *)
  OMX_IndexConfigTimePosition              = $0900000A;  (* reference: OMX_TIME_CONFIG_TIMESTAMPTYPE *)
  OMX_IndexConfigTimeSeekMode              = $0900000B;  (* reference: OMX_TIME_CONFIG_SEEKMODETYPE *)

  OMX_IndexKhronosExtensions               = $6F000000;  (* Reserved region for introducing Khronos Standard Extensions *)
  (* Vendor specific area *)

  OMX_IndexVendorStartUnused               = $7F000000;
  (* Vendor specific structures should be in the range of 0x7F000000
     to 0x7FFFFFFE.  This range is not broken out by vendor, so
     private indexes are not guaranteed unique and therefore should
     only be sent to the appropriate component. *)

  (* used for ilcs-top communication *)
  OMX_IndexParamMarkComparison             = $7F000001;  (* reference: OMX_PARAM_MARKCOMPARISONTYPE *)
  OMX_IndexParamPortSummary                = $7F000002;  (* reference: OMX_PARAM_PORTSUMMARYTYPE *)
  OMX_IndexParamTunnelStatus               = $7F000003;  (* reference: OMX_PARAM_TUNNELSTATUSTYPE *)
  OMX_IndexParamBrcmRecursionUnsafe        = $7F000004;  (* reference: OMX_PARAM_BRCMRECURSIONUNSAFETYPE *)

  (* used for top-ril communication *)
  OMX_IndexParamBufferAddress              = $7F000005;  (* reference : OMX_PARAM_BUFFERADDRESSTYPE *)
  OMX_IndexParamTunnelSetup                = $7F000006;  (* reference : OMX_PARAM_TUNNELSETUPTYPE *)
  OMX_IndexParamBrcmPortEGL                = $7F000007;  (* reference : OMX_PARAM_BRCMPORTEGLTYPE *)
  OMX_IndexParamIdleResourceCount          = $7F000008;  (* reference : OMX_PARAM_U32TYPE *)

  (* used for ril-ril communication *)
  OMX_IndexParamImagePoolDisplayFunction   = $7F000009;  (* reference : OMX_PARAM_IMAGEDISPLAYFUNCTIONTYPE *)
  OMX_IndexParamBrcmDataUnit               = $7F00000A;  (* reference: OMX_PARAM_DATAUNITTYPE *)
  OMX_IndexParamCodecConfig                = $7F00000B;  (* reference: OMX_PARAM_CODECCONFIGTYPE *)
  OMX_IndexParamCameraPoolToEncoderFunction = $7F00000C; (* reference : OMX_PARAM_CAMERAPOOLTOENCODERFUNCTIONTYPE *)
  OMX_IndexParamCameraStripeFunction       = $7F00000D;  (* reference : OMX_PARAM_CAMERASTRIPEFUNCTIONTYPE *)
  OMX_IndexParamCameraCaptureEventFunction = $7F00000E;  (* reference : OMX_PARAM_CAMERACAPTUREEVENTFUNCTIONTYPE *)

  (* used for client-ril communication *)
  OMX_IndexParamTestInterface              = $7F00000F;  (* reference : OMX_PARAM_TESTINTERFACETYPE *)

  // 0x7f000010
  OMX_IndexConfigDisplayRegion             = $7f000010;  (* reference : OMX_CONFIG_DISPLAYREGIONTYPE *)
  OMX_IndexParamSource                     = $7f000011;  (* reference : OMX_PARAM_SOURCETYPE *)
  OMX_IndexParamSourceSeed                 = $7f000012;  (* reference : OMX_PARAM_SOURCESEEDTYPE *)
  OMX_IndexParamResize                     = $7f000013;  (* reference : OMX_PARAM_RESIZETYPE *)
  OMX_IndexConfigVisualisation             = $7f000014;  (* reference : OMX_CONFIG_VISUALISATIONTYPE *)
  OMX_IndexConfigSingleStep                = $7f000015;  (* reference : OMX_PARAM_U32TYPE *)
  OMX_IndexConfigPlayMode                  = $7f000016;  (* reference: OMX_CONFIG_PLAYMODETYPE *)
  OMX_IndexParamCameraCamplusId            = $7f000017;  (* reference : OMX_PARAM_U32TYPE *)
  OMX_IndexConfigCommonImageFilterParameters = $7f000018; (* reference : OMX_CONFIG_IMAGEFILTERPARAMSTYPE *)
  OMX_IndexConfigTransitionControl         = $7f000019;  (* reference : OMX_CONFIG_TRANSITIONCONTROLTYPE *)
  OMX_IndexConfigPresentationOffset        = $7f00001A;  (* reference: OMX_TIME_CONFIG_TIMESTAMPTYPE *)
  OMX_IndexParamSourceFunctions            = $7f00001B;  (* reference: OMX_PARAM_STILLSFUNCTIONTYPE *)
  OMX_IndexConfigAudioMonoTrackControl     = $7f00001C;  (* reference : OMX_CONFIG_AUDIOMONOTRACKCONTROLTYPE *)
  OMX_IndexParamCameraImagePool            = $7f00001D;  (* reference : OMX_PARAM_CAMERAIMAGEPOOLTYPE *)
  OMX_IndexConfigCameraISPOutputPoolHeight = $7f00001E;  (* reference : OMX_PARAM_U32TYPE *)
  OMX_IndexParamImagePoolSize              = $7f00001F;  (* reference: OMX_PARAM_IMAGEPOOLSIZETYPE *)

  // 0x7f000020
  OMX_IndexParamImagePoolExternal          = $7f000020;  (* reference: OMX_PARAM_IMAGEPOOLEXTERNALTYPE *)
  OMX_IndexParamRUTILFifoInfo              = $7f000021;  (* reference: OMX_PARAM_RUTILFIFOINFOTYPE*)
  OMX_IndexParamILFifoConfig               = $7f000022;  (* reference: OMX_PARAM_ILFIFOCONFIG *)
  OMX_IndexConfigCameraSensorModes         = $7f000023;  (* reference : OMX_CONFIG_CAMERASENSORMODETYPE *)
  OMX_IndexConfigBrcmPortStats             = $7f000024;  (* reference : OMX_CONFIG_BRCMPORTSTATSTYPE *)
  OMX_IndexConfigBrcmPortBufferStats       = $7f000025;  (* reference : OMX_CONFIG_BRCMPORTBUFFERSTATSTYPE *)
  OMX_IndexConfigBrcmCameraStats           = $7f000026;  (* reference : OMX_CONFIG_BRCMCAMERASTATSTYPE *)
  OMX_IndexConfigBrcmIOPerfStats           = $7f000027;  (* reference : OMX_CONFIG_BRCMIOPERFSTATSTYPE *)
  OMX_IndexConfigCommonSharpness           = $7f000028;  (* reference : OMX_CONFIG_SHARPNESSTYPE *)
  OMX_IndexConfigCommonFlickerCancellation = $7f000029;  (* reference : OMX_CONFIG_FLICKERCANCELTYPE *)
  OMX_IndexParamCameraSwapImagePools       = $7f00002A;  (* reference : OMX_CONFIG_BOOLEANTYPE *)
  OMX_IndexParamCameraSingleBufferCaptureInput = $7f00002B; (* reference : OMX_CONFIG_BOOLEANTYPE *)
  OMX_IndexConfigCommonRedEyeRemoval       = $7f00002C;  (* reference : OMX_CONFIG_REDEYEREMOVALTYPE  *)
  OMX_IndexConfigCommonFaceDetectionControl = $7f00002D; (* reference : OMX_CONFIG_FACEDETECTIONCONTROLTYPE *)
  OMX_IndexConfigCommonFaceDetectionRegion = $7f00002E;  (* reference : OMX_CONFIG_FACEDETECTIONREGIONTYPE *)
  OMX_IndexConfigCommonInterlace           = $7f00002F;  (* reference: OMX_CONFIG_INTERLACETYPE *)

  // 0x7f000030
  OMX_IndexParamISPTunerName               = $7f000030;  (* reference: OMX_PARAM_CAMERAISPTUNERTYPE *)
  OMX_IndexParamCameraDeviceNumber         = $7f000031;  (* reference: OMX_PARAM_U32TYPE *)
  OMX_IndexParamCameraDevicesPresent       = $7f000032;  (* reference: OMX_PARAM_U32TYPE *)
  OMX_IndexConfigCameraInputFrame          = $7f000033;  (* reference: OMX_CONFIG_IMAGEPTRTYPE *)
  OMX_IndexConfigStillColourDenoiseEnable  = $7f000034;  (* reference: OMX_CONFIG_BOOLEANTYPE *)
  OMX_IndexConfigVideoColourDenoiseEnable  = $7f000035;  (* reference: OMX_CONFIG_BOOLEANTYPE *)
  OMX_IndexConfigAFAssistLight             = $7f000036;  (* reference: OMX_CONFIG_AFASSISTTYPE *)
  OMX_IndexConfigSmartShakeReductionEnable = $7f000037;  (* reference: OMX_CONFIG_BOOLEANTYPE *)
  OMX_IndexConfigInputCropPercentages      = $7f000038;  (* reference: OMX_CONFIG_INPUTCROPTYPE *)
  OMX_IndexConfigStillsAntiShakeEnable     = $7f000039;  (* reference: OMX_CONFIG_BOOLEANTYPE *)
  OMX_IndexConfigWaitForFocusBeforeCapture = $7f00003A;  (* reference: OMX_CONFIG_BOOLEANTYPE *)
  OMX_IndexConfigAudioRenderingLatency     = $7f00003B;  (* reference: OMX_PARAM_U32TYPE *)
  OMX_IndexConfigDrawBoxAroundFaces        = $7f00003C;  (* reference: OMX_CONFIG_BOOLEANTYPE *)
  OMX_IndexParamCodecRequirements          = $7f00003D;  (* reference: OMX_PARAM_CODECREQUIREMENTSTYPE *)
  OMX_IndexConfigBrcmEGLImageMemHandle     = $7f00003E;  (* reference: OMX_CONFIG_BRCMEGLIMAGEMEMHANDLETYPE *)
  OMX_IndexConfigPrivacyIndicator          = $7f00003F;  (* reference: OMX_CONFIG_PRIVACYINDICATORTYPE *)

  // 0x7f000040
  OMX_IndexParamCameraFlashType            = $7f000040;  (* reference: OMX_PARAM_CAMERAFLASHTYPE *)
  OMX_IndexConfigCameraEnableStatsPass     = $7f000041;  (* reference: OMX_CONFIG_BOOLEANTYPE *)
  OMX_IndexConfigCameraFlashConfig         = $7f000042;  (* reference: OMX_CONFIG_CAMERAFLASHCONFIGTYPE *)
  OMX_IndexConfigCaptureRawImageURI        = $7f000043;  (* reference: OMX_PARAM_CONTENTURITYPE *)
  OMX_IndexConfigCameraStripeFuncMinLines  = $7f000044;  (* reference: OMX_PARAM_U32TYPE *)
  OMX_IndexConfigCameraAlgorithmVersionDeprecated = $7f000045; (* reference: OMX_PARAM_U32TYPE *)
  OMX_IndexConfigCameraIsoReferenceValue   = $7f000046;  (* reference: OMX_PARAM_U32TYPE *)
  OMX_IndexConfigCameraCaptureAbortsAutoFocus = $7f000047; (*reference: OMX_CONFIG_BOOLEANTYPE *)
  OMX_IndexConfigBrcmClockMissCount        = $7f000048;  (* reference: OMX_PARAM_U32TYPE *)
  OMX_IndexConfigFlashChargeLevel          = $7f000049;  (* reference: OMX_PARAM_U32TYPE *)
  OMX_IndexConfigBrcmVideoEncodedSliceSize = $7f00004A;  (* reference: OMX_PARAM_U32TYPE *)
  OMX_IndexConfigBrcmAudioTrackGaplessPlayback = $7f00004B; (* reference: OMX_CONFIG_BRCMAUDIOTRACKGAPLESSPLAYBACKTYPE *)
  OMX_IndexConfigBrcmAudioTrackChangeControl = $7f00004C; (* reference: OMX_CONFIG_BRCMAUDIOTRACKCHANGECONTROLTYPE *)
  OMX_IndexParamBrcmPixelAspectRatio       = $7f00004D;  (* reference: OMX_CONFIG_POINTTYPE *)
  OMX_IndexParamBrcmPixelValueRange        = $7f00004E;  (* reference: OMX_PARAM_BRCMPIXELVALUERANGETYPE *)
  OMX_IndexParamCameraDisableAlgorithm     = $7f00004F;  (* reference: OMX_PARAM_CAMERADISABLEALGORITHMTYPE *)

  // 0x7f000050
  OMX_IndexConfigBrcmVideoIntraPeriodTime  = $7f000050;  (* reference: OMX_PARAM_U32TYPE *)
  OMX_IndexConfigBrcmVideoIntraPeriod      = $7f000051;  (* reference: OMX_PARAM_U32TYPE *)
  OMX_IndexConfigBrcmAudioEffectControl    = $7f000052;  (* reference: OMX_CONFIG_BRCMAUDIOEFFECTCONTROLTYPE *)
  OMX_IndexConfigBrcmMinimumProcessingLatency = $7f000053; (* reference: OMX_CONFIG_BRCMMINIMUMPROCESSINGLATENCY *)
  OMX_IndexParamBrcmVideoAVCSEIEnable      = $7f000054;  (* reference: OMX_PARAM_BRCMVIDEOAVCSEIENABLETYPE *)
  OMX_IndexParamBrcmAllowMemChange         = $7f000055;  (* reference: OMX_PARAM_BRCMALLOWMEMCHANGETYPE *)
  OMX_IndexConfigBrcmVideoEncoderMBRowsPerSlice = $7f000056; (* reference: OMX_PARAM_U32TYPE *)
  OMX_IndexParamCameraAFAssistDeviceNumber_Deprecated = $7f000057; (* reference: OMX_PARAM_U32TYPE *)
  OMX_IndexParamCameraPrivacyIndicatorDeviceNumber_Deprecated = $7f000058; (* reference: OMX_PARAM_U32TYPE *)
  OMX_IndexConfigCameraUseCase             = $7f000059;  (* reference: OMX_CONFIG_CAMERAUSECASETYPE *)
  OMX_IndexParamBrcmDisableProprietaryTunnels = $7f00005A; (* reference: OMX_PARAM_BRCMDISABLEPROPRIETARYTUNNELSTYPE *)
  OMX_IndexParamBrcmOutputBufferSize       = $7f00005B;  (*  reference: OMX_PARAM_BRCMOUTPUTBUFFERSIZETYPE *)
  OMX_IndexParamBrcmRetainMemory           = $7f00005C;  (* reference: OMX_PARAM_BRCMRETAINMEMORYTYPE *)
  OMX_IndexConfigCanFocus_Deprecated       = $7f00005D;  (* reference: OMX_PARAM_U32TYPE *)
  OMX_IndexParamBrcmImmutableInput         = $7f00005E;  (* reference: OMX_CONFIG_BOOLEANTYPE *)
  OMX_IndexParamDynamicParameterFile       = $7f00005F;  (* reference: OMX_PARAM_CONTENTURITYPE *)

  // 0x7f000060
  OMX_IndexParamUseDynamicParameterFile    = $7f000060;  (* reference: OMX_CONFIG_BOOLEANTYPE *)
  OMX_IndexConfigCameraInfo                = $7f000061;  (* reference: OMX_CONFIG_CAMERAINFOTYPE *)
  OMX_IndexConfigCameraFeatures            = $7f000062;  (* reference: OMX_CONFIG_CAMERAFEATURESTYPE *)
  OMX_IndexConfigRequestCallback           = $7f000063;  (* reference: OMX_CONFIG_REQUESTCALLBACKTYPE *) //Should be added to the spec as part of IL416c
  OMX_IndexConfigBrcmOutputBufferFullCount = $7f000064;  (* reference: OMX_PARAM_U32TYPE *)
  OMX_IndexConfigCommonFocusRegionXY       = $7f000065;  (* reference: OMX_CONFIG_FOCUSREGIONXYTYPE *)
  OMX_IndexParamBrcmDisableEXIF            = $7f000066;  (* reference: OMX_CONFIG_BOOLEANTYPE *)
  OMX_IndexConfigUserSettingsId            = $7f000067;  (* reference: OMX_CONFIG_U8TYPE *)
  OMX_IndexConfigCameraSettings            = $7f000068;  (* reference: OMX_CONFIG_CAMERASETTINGSTYPE *)
  OMX_IndexConfigDrawBoxLineParams         = $7f000069;  (* reference: OMX_CONFIG_DRAWBOXLINEPARAMS *)
  OMX_IndexParamCameraRmiControl_Deprecated = $7f00006A; (* reference: OMX_PARAM_CAMERARMITYPE *)
  OMX_IndexConfigBurstCapture              = $7f00006B;  (* reference: OMX_CONFIG_BOOLEANTYPE *)
  OMX_IndexParamBrcmEnableIJGTableScaling  = $7f00006C;  (* reference: OMX_PARAM_IJGSCALINGTYPE *)
  OMX_IndexConfigPowerDown                 = $7f00006D;  (* reference: OMX_CONFIG_BOOLEANTYPE *)
  OMX_IndexConfigBrcmSyncOutput            = $7f00006E;  (* reference: OMX_CONFIG_BRCMSYNCOUTPUTTYPE *)
  OMX_IndexParamBrcmFlushCallback          = $7f00006F;  (* reference: OMX_PARAM_BRCMFLUSHCALLBACK *)
                                               {
  // 0x7f000070
  OMX_IndexConfigBrcmVideoRequestIFrame,     (* reference: OMX_CONFIG_BOOLEANTYPE *)
  OMX_IndexParamBrcmNALSSeparate,            (* reference: OMX_CONFIG_BOOLEANTYPE *)
  OMX_IndexConfigConfirmView,                (* reference: OMX_CONFIG_BOOLEANTYPE *)
  OMX_IndexConfigDrmView,                    (* reference: OMX_CONFIG_DRMVIEWTYPE *)
  OMX_IndexConfigBrcmVideoIntraRefresh,      (* reference: OMX_VIDEO_PARAM_INTRAREFRESHTYPE *)
  OMX_IndexParamBrcmMaxFileSize,             (* reference: OMX_PARAM_BRCMU64TYPE *)
  OMX_IndexParamBrcmCRCEnable,               (* reference: OMX_CONFIG_BOOLEANTYPE *)
  OMX_IndexParamBrcmCRC,                     (* reference: OMX_PARAM_U32TYPE *)
  OMX_IndexConfigCameraRmiInUse_Deprecated,             (* reference: OMX_CONFIG_BOOLEANTYPE *)
  OMX_IndexConfigBrcmAudioSource,            (*reference: OMX_CONFIG_BRCMAUDIOSOURCETYPE *)
  OMX_IndexConfigBrcmAudioDestination,       (* reference: OMX_CONFIG_BRCMAUDIODESTINATIONTYPE *)
  OMX_IndexParamAudioDdp,                    (* reference: OMX_AUDIO_PARAM_DDPTYPE *)
  OMX_IndexParamBrcmThumbnail,               (* reference: OMX_PARAM_BRCMTHUMBNAILTYPE *)
  OMX_IndexParamBrcmDisableLegacyBlocks_Deprecated,     (* reference: OMX_CONFIG_BOOLEANTYPE *)
  OMX_IndexParamBrcmCameraInputAspectRatio,  (* reference: OMX_PARAM_BRCMASPECTRATIOTYPE *)
  OMX_IndexParamDynamicParameterFileFailFatal,(* reference: OMX_CONFIG_BOOLEANTYPE *)

  // 0x7f000080
  OMX_IndexParamBrcmVideoDecodeErrorConcealment, (* reference: OMX_PARAM_BRCMVIDEODECODEERRORCONCEALMENTTYPE *)
  OMX_IndexParamBrcmInterpolateMissingTimestamps, (* reference: OMX_CONFIG_BOOLEANTYPE *)
  OMX_IndexParamBrcmSetCodecPerformanceMonitoring, (* reference: OMX_PARAM_U32TYPE *)
  OMX_IndexConfigFlashInfo,                  (* reference: OMX_CONFIG_FLASHINFOTYPE *)
  OMX_IndexParamBrcmMaxFrameSkips,           (* reference: OMX_PARAM_U32TYPE *)
  OMX_IndexConfigDynamicRangeExpansion,      (* reference: OMX_CONFIG_DYNAMICRANGEEXPANSIONTYPE *)
  OMX_IndexParamBrcmFlushCallbackId,         (* reference: OMX_PARAM_U32TYPE *)
  OMX_IndexParamBrcmTransposeBufferCount,    (* reference: OMX_PARAM_U32TYPE *)
  OMX_IndexConfigFaceRecognitionControl,     (* reference: OMX_CONFIG_BOOLEANTYPE *)
  OMX_IndexConfigFaceRecognitionSaveFace,    (* reference: OMX_PARAM_BRCMU64TYPE *)
  OMX_IndexConfigFaceRecognitionDatabaseUri, (* reference: OMX_PARAM_CONTENTURITYPE *)
  OMX_IndexConfigClockAdjustment,            (* reference: OMX_TIME_CONFIG_TIMESTAMPTYPE *)
  OMX_IndexParamBrcmThreadAffinity,          (* reference: OMX_PARAM_BRCMTHREADAFFINITYTYPE *)
  OMX_IndexParamAsynchronousOutput,          (* reference: OMX_CONFIG_BOOLEANTYPE *)
  OMX_IndexConfigAsynchronousFailureURI,     (* reference: OMX_PARAM_CONTENTURITYPE *)
  OMX_IndexConfigCommonFaceBeautification,   (* reference: OMX_CONFIG_BOOLEANTYPE *)

  // 0x7f000090
  OMX_IndexConfigCommonSceneDetectionControl = $7f000090; (* reference: OMX_CONFIG_BOOLEANTYPE *)
  OMX_IndexConfigCommonSceneDetected       = $7f000091;  (* reference: OMX_CONFIG_SCENEDETECTTYPE *)
  OMX_IndexParamDisableVllPool             = $7f000092;  (* reference: OMX_CONFIG_BOOLEANTYPE *)
  OMX_IndexParamVideoMvc                   = $7f000093;  (* reference: OMX_VIDEO_PARAM_MVCTYPE *)
  OMX_IndexConfigBrcmDrawStaticBox         = $7f000094;  (* reference: OMX_CONFIG_STATICBOXTYPE *)
  OMX_IndexConfigBrcmClockReferenceSource  = $7f000095;  (* reference: OMX_CONFIG_BOOLEANTYPE *)
  OMX_IndexParamPassBufferMarks            = $7f000096;  (* reference: OMX_CONFIG_BOOLEANTYPE *)
  OMX_IndexConfigPortCapturing             = $7f000097;  (* reference: OMX_CONFIG_PORTBOOLEANTYPE *)
  OMX_IndexConfigBrcmDecoderPassThrough    = $7f000098;  (* reference: OMX_CONFIG_BOOLEANTYPE *)
  OMX_IndexParamBrcmDecoderPassThrough     = OMX_IndexConfigBrcmDecoderPassThrough;  (* deprecated *)
  OMX_IndexParamBrcmMaxCorruptMBs          = $7f000099;  (* reference: OMX_PARAM_U32TYPE *)
  OMX_IndexConfigBrcmGlobalAudioMute       = $7f00009A;  (* reference: OMX_CONFIG_BOOLEANTYPE *)
  OMX_IndexParamCameraCaptureMode          = $7f00009B;  (* reference: OMX_PARAM_CAMERACAPTUREMODETYPE *)
  OMX_IndexParamBrcmDrmEncryption          = $7f00009C;  (* reference: OMX_PARAM_BRCMDRMENCRYPTIONTYPE *)
  OMX_IndexConfigBrcmCameraRnDPreprocess   = $7f00009D;  (* reference: OMX_CONFIG_BOOLEANTYPE *)
  OMX_IndexConfigBrcmCameraRnDPostprocess  = $7f00009E;  (* reference: OMX_CONFIG_BOOLEANTYPE *)
  OMX_IndexConfigBrcmAudioTrackChangeCount = $7f00009F;  (* reference: OMX_PARAM_U32TYPE *)

  // 0x7f0000a0
  OMX_IndexParamCommonUseStcTimestamps,      (* reference: OMX_PARAM_TIMESTAMPMODETYPE *)
  OMX_IndexConfigBufferStall,                (* reference: OMX_CONFIG_BUFFERSTALLTYPE *)
  OMX_IndexConfigRefreshCodec,               (* reference: OMX_CONFIG_BOOLEANTYPE *)
  OMX_IndexParamCaptureStatus,               (* reference: OMX_PARAM_CAPTURESTATETYPE *)
  OMX_IndexConfigTimeInvalidStartTime,       (* reference: OMX_TIME_CONFIG_TIMESTAMPTYPE *)
  OMX_IndexConfigLatencyTarget,              (* reference: OMX_CONFIG_LATENCYTARGETTYPE *)
  OMX_IndexConfigMinimiseFragmentation,      (* reference: OMX_CONFIG_BOOLEANTYPE *)
  OMX_IndexConfigBrcmUseProprietaryCallback, (* reference: OMX_CONFIG_BRCMUSEPROPRIETARYTUNNELTYPE *)
  OMX_IndexParamPortMaxFrameSize,            (* reference: OMX_FRAMESIZETYPE *)
  OMX_IndexParamComponentName,               (* reference: OMX_PARAM_COMPONENTROLETYPE *)
  OMX_IndexConfigEncLevelExtension,          (* reference: OMX_VIDEO_CONFIG_LEVEL_EXTEND *)
  OMX_IndexConfigTemporalDenoiseEnable,      (* reference: OMX_CONFIG_BOOLEANTYPE *)
  OMX_IndexParamBrcmLazyImagePoolDestroy,    (* reference: OMX_CONFIG_BOOLEANTYPE *)
  OMX_IndexParamBrcmEEDEEnable,              (* reference: OMX_VIDEO_EEDE_ENABLE *)
  OMX_IndexParamBrcmEEDELossRate,            (* reference: OMX_VIDEO_EEDE_LOSSRATE *)
  OMX_IndexParamAudioDts,                    (* reference: OMX_AUDIO_PARAM_DTSTYPE *)

  // 0x7f0000b0
  OMX_IndexParamNumOutputChannels,           (* reference: OMX_PARAM_U32TYPE *)
  OMX_IndexConfigBrcmHighDynamicRange,       (* reference: OMX_CONFIG_BOOLEANTYPE *)
  OMX_IndexConfigBrcmPoolMemAllocSize,       (* reference: OMX_PARAM_U32TYPE *)
  OMX_IndexConfigBrcmBufferFlagFilter,       (* reference: OMX_PARAM_U32TYPE *)
  OMX_IndexParamBrcmVideoEncodeMinQuant,     (* reference: OMX_PARAM_U32TYPE *)
  OMX_IndexParamBrcmVideoEncodeMaxQuant,     (* reference: OMX_PARAM_U32TYPE *)
  OMX_IndexParamRateControlModel,            (* reference: OMX_PARAM_U32TYPE *)
  OMX_IndexParamBrcmExtraBuffers,            (* reference: OMX_PARAM_U32TYPE *)
  OMX_IndexConfigFieldOfView,                (* reference: OMX_CONFIG_BRCMFOVTYPE *)
  OMX_IndexParamBrcmAlignHoriz,              (* reference: OMX_PARAM_U32TYPE *)
  OMX_IndexParamBrcmAlignVert,               (* reference: OMX_PARAM_U32TYPE *)
  OMX_IndexParamColorSpace,                  (* reference: OMX_PARAM_COLORSPACETYPE *)
  OMX_IndexParamBrcmDroppablePFrames,        (* reference: OMX_CONFIG_BOOLEANTYPE *)
  OMX_IndexParamBrcmVideoInitialQuant,       (* reference: OMX_PARAM_U32TYPE *)
  OMX_IndexParamBrcmVideoEncodeQpP,          (* reference: OMX_PARAM_U32TYPE *)
  OMX_IndexParamBrcmVideoRCSliceDQuant,      (* reference: OMX_PARAM_U32TYPE *)

  // 0x7f0000c0
  OMX_IndexParamBrcmVideoFrameLimitBits,     (* reference: OMX_PARAM_U32TYPE *)
  OMX_IndexParamBrcmVideoPeakRate,           (* reference: OMX_PARAM_U32TYPE *)
  OMX_IndexConfigBrcmVideoH264DisableCABAC,  (* reference: OMX_CONFIG_BOOLEANTYPE *)
  OMX_IndexConfigBrcmVideoH264LowLatency,    (* reference: OMX_CONFIG_BOOLEANTYPE *)
  OMX_IndexConfigBrcmVideoH264AUDelimiters,  (* reference: OMX_CONFIG_BOOLEANTYPE *)
  OMX_IndexConfigBrcmVideoH264DeblockIDC,    (* reference: OMX_PARAM_U32TYPE *)
  OMX_IndexConfigBrcmVideoH264IntraMBMode,   (* reference: OMX_PARAM_U32TYPE *)
  OMX_IndexConfigContrastEnhance,            (* reference: OMX_CONFIG_BOOLEANTYPE *)
  OMX_IndexParamCameraCustomSensorConfig,    (* reference: OMX_PARAM_U32TYPE *)
  OMX_IndexParamBrcmHeaderOnOpen,            (* reference: OMX_CONFIG_BOOLEANTYPE *)
  OMX_IndexConfigBrcmUseRegisterFile,        (* reference: OMX_CONFIG_BOOLEANTYPE *)
  OMX_IndexConfigBrcmRegisterFileFailFatal,  (* reference: OMX_CONFIG_BOOLEANTYPE *)
  OMX_IndexParamBrcmConfigFileRegisters,     (* reference: OMX_PARAM_BRCMCONFIGFILETYPE *)
  OMX_IndexParamBrcmConfigFileChunkRegisters,(* reference: OMX_PARAM_BRCMCONFIGFILECHUNKTYPE *)
  OMX_IndexParamBrcmAttachLog,               (* reference: OMX_CONFIG_BOOLEANTYPE *)
  OMX_IndexParamCameraZeroShutterLag,        (* reference: OMX_CONFIG_ZEROSHUTTERLAGTYPE *)

  // 0x7f0000d0
  OMX_IndexParamBrcmFpsRange,                (* reference: OMX_PARAM_BRCMFRAMERATERANGETYPE *)
  OMX_IndexParamCaptureExposureCompensation, (* reference: OMX_PARAM_S32TYPE *)
  OMX_IndexParamBrcmVideoPrecodeForQP,       (* reference: OMX_CONFIG_BOOLEANTYPE *)
  OMX_IndexParamBrcmVideoTimestampFifo,      (* reference: OMX_CONFIG_BOOLEANTYPE *)
  OMX_IndexParamSWSharpenDisable,            (* reference: OMX_CONFIG_BOOLEANTYPE *)
  OMX_IndexConfigBrcmFlashRequired,          (* reference: OMX_CONFIG_BOOLEANTYPE *)
  OMX_IndexParamBrcmVideoDrmProtectBuffer,   (* reference: OMX_PARAM_BRCMVIDEODRMPROTECTBUFFERTYPE *)
  OMX_IndexParamSWSaturationDisable,         (* reference: OMX_CONFIG_BOOLEANTYPE *)
  OMX_IndexParamBrcmVideoDecodeConfigVD3,    (* reference: OMX_PARAM_BRCMVIDEODECODECONFIGVD3TYPE *)
  OMX_IndexConfigBrcmPowerMonitor,           (* reference: OMX_CONFIG_BOOLEANTYPE *)
  OMX_IndexParamBrcmZeroCopy,                (* reference: OMX_CONFIG_PORTBOOLEANTYPE *)
  OMX_IndexParamBrcmVideoEGLRenderDiscardMode,   (* reference: OMX_CONFIG_PORTBOOLEANTYPE *)
  OMX_IndexParamBrcmVideoAVC_VCLHRDEnable,    (* reference: OMX_CONFIG_PORTBOOLEANTYPE*)
  OMX_IndexParamBrcmVideoAVC_LowDelayHRDEnable, (* reference: OMX_CONFIG_PORTBOOLEANTYPE*)
  OMX_IndexParamBrcmVideoCroppingDisable,    (* reference: OMX_CONFIG_PORTBOOLEANTYPE*)
  OMX_IndexParamBrcmVideoAVCInlineHeaderEnable, (* reference: OMX_CONFIG_PORTBOOLEANTYPE*)

  // 0x7f0000f0
  OMX_IndexConfigBrcmAudioDownmixCoefficients = $7f0000f0; (* reference: OMX_CONFIG_BRCMAUDIODOWNMIXCOEFFICIENTS *)
  OMX_IndexConfigBrcmAudioDownmixCoefficients8x8,           (* reference: OMX_CONFIG_BRCMAUDIODOWNMIXCOEFFICIENTS8x8 *)
  OMX_IndexConfigBrcmAudioMaxSample,                        (* reference: OMX_CONFIG_BRCMAUDIOMAXSAMPLE *)
  OMX_IndexConfigCustomAwbGains,                            (* reference: OMX_CONFIG_CUSTOMAWBGAINSTYPE *)
  OMX_IndexParamRemoveImagePadding,                         (* reference: OMX_CONFIG_PORTBOOLEANTYPE*)
  OMX_IndexParamBrcmVideoAVCInlineVectorsEnable,            (* reference: OMX_CONFIG_PORTBOOLEANTYPE *)
  OMX_IndexConfigBrcmRenderStats,                           (* reference: OMX_CONFIG_BRCMRENDERSTATSTYPE *)
  OMX_IndexConfigBrcmCameraAnnotate,                        (* reference: OMX_CONFIG_BRCMANNOTATETYPE *)
  OMX_IndexParamBrcmStereoscopicMode,                       (* reference :OMX_CONFIG_BRCMSTEREOSCOPICMODETYPE *)
  OMX_IndexParamBrcmLockStepEnable,                         (* reference: OMX_CONFIG_PORTBOOLEANTYPE *)
  OMX_IndexParamBrcmTimeScale,                              (* reference: OMX_PARAM_U32TYPE *)
  OMX_IndexParamCameraInterface,                            (* reference: OMX_PARAM_CAMERAINTERFACETYPE *)
  OMX_IndexParamCameraClockingMode,                         (* reference: OMX_PARAM_CAMERACLOCKINGMODETYPE *)
  OMX_IndexParamCameraRxConfig,                             (* reference: OMX_PARAM_CAMERARXCONFIG_TYPE *)
  OMX_IndexParamCameraRxTiming,                             (* reference: OMX_PARAM_CAMERARXTIMING_TYPE *)
  OMX_IndexParamDynamicParameterConfig,                     (* reference: OMX_PARAM_U32TYPE *)

  // 0x7f000100
  OMX_IndexParamBrcmVideoAVCSPSTimingEnable,                (* reference: OMX_CONFIG_PORTBOOLEANTYPE *)
  OMX_IndexParamBrcmBayerOrder,                             (* reference: OMX_PARAM_BAYERORDERTYPE *)
  OMX_IndexParamBrcmMaxNumCallbacks,                        (* reference: OMX_PARAM_U32TYPE *)
  OMX_IndexParamBrcmJpegRestartInterval,                    (* reference: OMX_PARAM_U32TYPE *)
  OMX_IndexParamBrcmSupportsSlices,                         (* reference: OMX_CONFIG_PORTBOOLEANTYPE *)
  OMX_IndexParamBrcmIspBlockOverride,                       (* reference: OMX_PARAM_U32TYPE *)
  OMX_IndexParamBrcmSupportsUnalignedSliceheight,           (* reference: OMX_CONFIG_PORTBOOLEANTYPE *)
  OMX_IndexParamBrcmLensShadingOverride,                    (* reference: OMX_PARAM_LENSSHADINGOVERRIDETYPE *)
  OMX_IndexParamBrcmBlackLevel,                             (* reference: OMX_PARAM_U32TYPE *)
  OMX_IndexParamOutputShift,                                (* reference: OMX_PARAM_S32TYPE *)
  OMX_IndexParamCcmShift,                                   (* reference: OMX_PARAM_S32TYPE *)
  OMX_IndexParamCustomCcm,                                  (* reference: OMX_PARAM_CUSTOMCCMTYPE *)
  OMX_IndexMax = $7FFFFFFF;
                   }

function OMX_Init : OMX_ERRORTYPE; cdecl; external;
function OMX_Deinit : OMX_ERRORTYPE; cdecl; external;
function OMX_GetHandle (pHandle : POMX_HANDLETYPE;
                        cComponentName : OMX_STRING;
                        pAppData : OMX_PTR;
                        pCallBacks : POMX_CALLBACKTYPE) : OMX_ERRORTYPE; cdecl; external;

function OMX_ComponentNameEnum (cComponentName : OMX_STRING;
                                nNameLength : OMX_U32;
                                nIndex : OMX_U32) : OMX_ERRORTYPE; cdecl; external;
function OMX_GetRolesOfComponent (compName : OMX_STRING;
                                  pNumRoles : POMX_U32;
                                  roles : PPOMX_U8) : OMX_ERRORTYPE; cdecl; external;

// macros
function OMX_GetState (hComponent : OMX_HANDLETYPE; pState : POMX_STATETYPE) : OMX_ERRORTYPE;

function OMX_GetParameter (hComponent : OMX_HANDLETYPE;
                           nParamIndex : OMX_INDEXTYPE;
                           pComponentParameterStructure : OMX_PTR) : OMX_ERRORTYPE;

function OMX_SetParameter (hComponent : OMX_HANDLETYPE;
                           nParamIndex : OMX_INDEXTYPE;
                           pComponentParameterStructure : OMX_PTR) : OMX_ERRORTYPE;

function OMX_EmptyThisBuffer (hComponent : OMX_HANDLETYPE;
                              pBuffer : POMX_BUFFERHEADERTYPE) : OMX_ERRORTYPE; cdecl;

function OMX_FillThisBuffer (hComponent : OMX_HANDLETYPE;
                             pBuffer : POMX_BUFFERHEADERTYPE) : OMX_ERRORTYPE; cdecl;

function OMX_SendCommand (hComponent : OMX_HANDLETYPE;
                          Cmd : OMX_COMMANDTYPE;
                          nParam1 : OMX_U32;
                          pCmdData : OMX_PTR) : OMX_ERRORTYPE;

function OMX_ErrToStr (err : OMX_ERRORTYPE) : string;
function OMX_StateToStr (s : OMX_STATETYPE) : string;



implementation

// macros
function OMX_GetParameter (hComponent : OMX_HANDLETYPE;
                           nParamIndex : OMX_INDEXTYPE;
                           pComponentParameterStructure : OMX_PTR) : OMX_ERRORTYPE;
begin
  Result := POMX_COMPONENTTYPE (hCOmponent)^.GetParameter (hComponent, nParamIndex, pComponentParameterStructure);
end;

function OMX_SetParameter (hComponent : OMX_HANDLETYPE;
                           nParamIndex : OMX_INDEXTYPE;
                           pComponentParameterStructure : OMX_PTR) : OMX_ERRORTYPE;
begin
  Result := POMX_COMPONENTTYPE (hCOmponent)^.SetParameter (hComponent, nParamIndex, pComponentParameterStructure);
end;

function OMX_GetState (hComponent : OMX_HANDLETYPE; pState : POMX_STATETYPE) : OMX_ERRORTYPE;
begin
  Result := POMX_COMPONENTTYPE (hCOmponent)^.GetState (hComponent, pState);
end;

function OMX_SendCommand (hComponent : OMX_HANDLETYPE;
                          Cmd : OMX_COMMANDTYPE;
                          nParam1 : OMX_U32;
                          pCmdData : OMX_PTR) : OMX_ERRORTYPE;
begin
  Result := POMX_COMPONENTTYPE (hCOmponent)^.SendCommand (hComponent, Cmd, nParam1, pCmdData);
end;


function OMX_EmptyThisBuffer (hComponent : OMX_HANDLETYPE;
                              pBuffer : POMX_BUFFERHEADERTYPE) : OMX_ERRORTYPE; cdecl;
begin
  Result := POMX_COMPONENTTYPE (hCOmponent)^.EmptyThisBuffer (hComponent, pBuffer);
end;

function OMX_FillThisBuffer (hComponent : OMX_HANDLETYPE;
                             pBuffer : POMX_BUFFERHEADERTYPE) : OMX_ERRORTYPE; cdecl;
begin
  Result := POMX_COMPONENTTYPE (hCOmponent)^.FillThisBuffer (hComponent, pBuffer);
end;

function OMX_ErrToStr (err : OMX_ERRORTYPE) : string;
begin
  case err of
    OMX_ErrorNone                          : Result := 'OK';
    OMX_ErrorInsufficientResources         : Result := 'Insufficient Resources';
    OMX_ErrorUndefined                     : Result := 'Undefined';
    OMX_ErrorInvalidComponentName          : Result := 'Invalid Component Name';
    OMX_ErrorComponentNotFound             : Result := 'Component not Found';
    OMX_ErrorInvalidComponent              : Result := 'Invalid Component';
    OMX_ErrorBadParameter                  : Result := 'Bad Parameter';
    OMX_ErrorNotImplemented                : Result := 'Not Implemented';
    OMX_ErrorUnderflow                     : Result := 'Underflow';
    OMX_ErrorOverflow                      : Result := 'Overflow';
    OMX_ErrorHardware                      : Result := 'Hardware Error';
    OMX_ErrorInvalidState                  : Result := 'Invlaid State';
    OMX_ErrorStreamCorrupt                 : Result := 'Corrupt Stream';
    OMX_ErrorPortsNotCompatible            : Result := 'Ports not Compatible';
    OMX_ErrorResourcesLost                 : Result := 'Resources Lost';
    OMX_ErrorNoMore                        : Result := 'No More Indicies';
    OMX_ErrorVersionMismatch               : Result := 'Version Mismatch';
    OMX_ErrorNotReady                      : Result := 'Component not Ready';
    OMX_ErrorTimeout                       : Result := 'Timeout';
    OMX_ErrorSameState                     : Result := 'Same State';
    OMX_ErrorResourcesPreempted            : Result := 'Resources Preempted';
    OMX_ErrorPortUnresponsiveDuringAllocation : Result := 'Unresponsive during Allocation';
    OMX_ErrorPortUnresponsiveDuringDeallocation : Result := 'Unresponsive during Deallocation';
    OMX_ErrorPortUnresponsiveDuringStop    : Result := 'Unresponsive during Stop';
    OMX_ErrorIncorrectStateTransition      : Result := 'Incorrect State Transition';
    OMX_ErrorIncorrectStateOperation       : Result := 'Incorrect State Operation';
    OMX_ErrorUnsupportedSetting            : Result := 'Unsupported Setting';
    OMX_ErrorUnsupportedIndex              : Result := 'Unsupported Index';
    OMX_ErrorBadPortIndex                  : Result := 'Bad Port Index';
    OMX_ErrorPortUnpopulated               : Result := 'Port Unpopulated';
    OMX_ErrorComponentSuspended            : Result := 'Comonet suspended';
    OMX_ErrorDynamicResourcesUnavailable   : Result := 'Dynamic Resources Unavailable';
    OMX_ErrorMbErrorsInFrame               : Result := 'Errors in Frame';
    OMX_ErrorFormatNotDetected             : Result := 'Format not Detected';
    OMX_ErrorContentPipeOpenFailed         : Result := 'Pipe Open Failed';
    OMX_ErrorContentPipeCreationFailed     : Result := 'Pipe Creation Failed';
    OMX_ErrorSeperateTablesUsed            : Result := 'Separate Table being Used';
    OMX_ErrorTunnelingUnsupported          : Result := 'Tunneling Unsupported';
    OMX_ErrorDiskFull                      : Result := 'Disk Full';
    OMX_ErrorMaxFileSize                   : Result := 'Max File Size Reached';
    OMX_ErrorDrmUnauthorised               : Result := 'DRM Unauthorised';
    OMX_ErrorDrmExpired                    : Result := 'DRM Expired';
    OMX_ErrorDrmGeneral                    : Result := 'General DRM Error';
    else                                     Result := 'Unknown Error (' + err.ToHexString (8) + ')';
  end;
end;

function OMX_StateToStr (s : OMX_STATETYPE) : string;
begin
  case s of
    OMX_StateInvalid          : Result := 'Invalid';
    OMX_StateLoaded           : Result := 'Loaded';
    OMX_StateIdle             : Result := 'Idle';
    OMX_StateExecuting        : Result := 'Executing';
    OMX_StatePause            : Result := 'Paused';
    OMX_StateWaitForResources : Result := 'Waiting for Resources';
    else                        Result := 'Unknown State (' + s.ToHexString (8) + ')';
    end;
end;

end.

