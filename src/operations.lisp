#|
 This file is a part of cl-visa32 project.
|#

(defpackage cl-visa32.operations
  (:use :cl :cffi)
  (:export :vi-assert-intr-signal
           :vi-assert-trigger
           :vi-assert-util-signal
           :vi-buf-read
           :vi-buf-write
           :vi-clear
           :vi-close
           :vi-disable-event
           :vi-discard-events
           :vi-enable-event
           :vi-event-handler
           :vi-find-next
           :vi-find-rsrc
           :vi-flush
           :vi-get-attribute
           :vi-gpib-command
           :vi-gpib-control-atn
           :vi-gpib-control-ren
           :vi-gpib-pass-control
           :vi-gpib-send-ifc
           :vi-in8
           :vi-in16
           :vi-in32
           :vi-install-handler
           :vi-lock
           :vi-map-address
           :vi-map-trigger
           :vi-mem-alloc
           :vi-mem-free
           :vi-move
           :vi-move-async
           :vi-move-in8
           :vi-move-in16
           :vi-move-in32
           :vi-move-out8
           :vi-move-out16
           :vi-move-out32
           :vi-open
           :vi-open-default-rm
           :vi-out8
           :vi-out16
           :vi-out32
           :vi-parse-rsrc
           :vi-parse-rsrc-ex
           :vi-peek8
           :vi-peek16
           :vi-peek32
           :vi-poke8
           :vi-poke16
           :vi-poke32
           :vi-printf
           :vi-queryf
           :vi-read
           :vi-read-async
           :vi-read-stb
           :vi-read-to-file
           :vi-scanf
           :vi-set-attribute
           :vi-set-buf
           :vi-sprintf
           :vi-sscanf
           :vi-status-desc
           :vi-terminate
           :vi-uninstall-handler
           :vi-unlock
           :vi-unmap-address
           :vi-unmap-trigger
           :vi-usb-control-in
           :vi-usb-control-out
           :vi-vprintf
           :vi-vqueryf
           :vi-vsscanf
           :vi-vxi-command-query
           :vi-wait-on-event
           :vi-write
           :vi-write-async
           :vi-write-from-file))

(in-package :cl-visa32.operations)

;; library

;; Set your visa32.dll library
; (pushnew #P"/lib/" *foreign-library-directories*)

(define-foreign-library visa32
    (:windows (:default "visa32")))

(use-foreign-library visa32)

;; operations

(defcfun ("viAssertIntrSignal" vi-assert-intr-signal)
  :long
   "Asserts the specified interrupt or signal."
  (vi :uint32)
  (mode :int16)
  (statusID :uint32))

(defcfun ("viAssertTrigger" vi-assert-trigger)
  :long
  "Asserts software or hardware trigger."
  (vi :uint32)
  (protocol :uint16))
  
(defcfun ("viAssertUtilSignal" vi-assert-util-signal)
  :long
  "Asserts or deasserts the specified utility bus signal."
  (vi :uint32)
  (line :uint16))

(defcfun ("viBufRead" vi-buf-read)
  :long
  "Reads data from device or interface through the use of a formatted I/O read buffer."
  (vi :uint32)
  (buf :pointer)
  (count :uint32)
  (retCount :pointer))

(defcfun ("viBufWrite" vi-buf-write)
  :long
  "Writes data to a formatted I/O write buffer synchronously."
  (vi :uint32)
  (buf :char)
  (count :uint32)
  (retCount :pointer))

(defcfun ("viClear" vi-clear)
  :long
  "Clears a device."
  (vi :uint32))

(defcfun ("viClose" vi-close)
  :long
  "Closes the specified session, event, or find list."
  (vi :uint32))

(defcfun ("viDisableEvent" vi-disable-event)
  :long
  "Disables notification of the specified event type(s) via the specified mechanism(s)."
  (vi :uint32)
  (eventType :long)
  (mechanism :uint16))

(defcfun ("viDiscardEvents" vi-discard-events)
  :long
  "Discards event occurrences for specified event types and mechanisms in a session."
  (vi :uint32)
  (eventType :long)
  (mechanism :uint16))

(defcfun ("viEnableEvent" vi-enable-event)
  :long
  "Enables notification of a specified event."
  (vi :uint32)
  (eventType :long)
  (mechanism :uint16)
  (contex :long))

(defcfun ("viEventHandler" vi-event-handler)
  :long
  "Event service handler procedure prototype."
  (vi :uint32)
  (eventType :long)
  (context :int)
  (userHandle (:pointer :void)))

(defcfun ("viFindNext" vi-find-next)
  :long
  "Returns the next resource from the list of resources found during a previous call to viFindRsrc()."
  (findList :uint32)
  (instrDesc :pointer))

(defcfun ("viFindRsrc" vi-find-rsrc)
  :long
  "Queries a VISA system to locate the resources associated with a specified interface."
  (sesn :uint32)
  (expr :char)
  (findList :pointer)
  (recnt :pointer)
  (instrDesc :pointer))

(defcfun ("viFlush" vi-flush)
  :long
  "Manually flushes the specified buffers associated with formatted I/O operations and/or serial communication."
  (vi :uint32)
  (mask :uint16))

(defcfun ("viGetAttribute" vi-get-attribute)
  :long
  "Retrieves the state of an attribute."
  (vi :uint32)
  (attribute :uint32)
  (attrState :pointer))

(defcfun ("viGpibCommand" vi-gpib-command)
  :long
  "Write GPIB command bytes on the bus."
  (vi :uint32)
  (buf :char)
  (count :uint32)
  (retCount :pointer))

(defcfun ("viGpibControlATN" vi-gpib-control-atn)
  :long
  "Specifies the state of the ATN line and the local active controller state."
  (vi :uint32)
  (mode :uint16))

(defcfun ("viGpibControlREN" vi-gpib-control-ren)
  :long
  "Controls the state of the GPIB Remote Enable (REN) interface line, and optionally the remote/local state of the device."
  (vi :uint32)
  (mode :uint16))

(defcfun ("viGpibPassControl" vi-gpib-pass-control)
  :long
  "Tell the GPIB device at the specified address to become controller in charge (CIC)."
  (vi :uint32)
  (primAddr :uint16)
  (secAddr :uint16))

(defcfun ("viGpibSendIFC" vi-gpib-send-ifc)
  :long
  "Pulse the interface clear line (IFC) for at least 100 microseconds."
  (vi :uint32))

(defcfun ("viIn8" vi-in8)
  :long
  "Reads in an 8-bit, 16-bit, or 32-bit value from the specified memory space and offset."
  (vi :uint32)
  (space :uint16)
  (offset :long)
  (val8 :pointer))

(defcfun ("viIn16" vi-in16)
  :long
  "Reads in an 8-bit, 16-bit, or 32-bit value from the specified memory space and offset."
  (vi :uint32)
  (space :uint16)
  (offset :long)
  (val16 :pointer))

(defcfun ("viIn32" vi-in32)
  :long
  "Reads in an 8-bit, 16-bit, or 32-bit value from the specified memory space and offset."
  (vi :uint32)
  (space :uint16)
  (offset :long)
  (val32 :pointer))

(defcfun ("viInstallHandler" vi-install-handler)
  :long
  "Installs handlers for event callbacks."
  (vi :uint32)
  (eventType :long)
  (handler :long)
  (userHandle (:pointer :void)))
  
(defcfun ("viLock" vi-lock)
  :long
  "Establishes an access mode to the specified resource."
  (vi :uint32)
  (lockType :long)
  (timeout :uint32)
  (requestedKey :string)
  (accessKey :pointer))

(defcfun ("viMapAddress" vi-map-address)
  :long
  "Maps the specified memory space into the process’s address space."
  (vi :uint32)
  (mapSpace :uint16)
  (mapBase :long)
  (mapSize :long)
  (access :uint16)
  (suggested (:pointer :void))
  (address :pointer))

(defcfun ("viMapTrigger" vi-map-trigger)
  :long
  "Map the specified trigger source line to the specified destination line."
  (vi :uint32)
  (trigSrc :int16)
  (trigDest :int16)
  (mode :uint16))

(defcfun ("viMemAlloc" vi-mem-alloc)
  :long
  "Allocates memory from a device’s memory region."
  (vi :uint32)
  (size :long)
  (offset :pointer))

(defcfun ("viMemFree" vi-mem-free)
  :long
  "Frees memory previously allocated using the viMemAlloc() operation."
  (vi :uint32)
  (offset :pointer))


(defcfun ("viMove" vi-move)
  :long
  "Moves a block of data."
  (vi :uint32)
  (srcSpace :uint16)
  (srcOffset :long)
  (srcWidth :uint16)
  (destSpace :uint16)
  (destOffset :long)
  (destWidth :uint16)
  (length :long))

(defcfun ("viMoveAsync" vi-move-async)
  :long
  "Moves a block of data asynchronously."
  (vi :uint32)
  (srcSpace :uint16)
  (srcOffset :long)
  (srcWidth :uint16)
  (destSpace :uint16)
  (destOffset :long)
  (destWidth :uint16)
  (length :long)
  (jobId :pointer))

(defcfun ("viMoveIn8" vi-move-in8)
  :long
  "Moves a block of data from the specified address space and offset to local memory."
  (vi :uint32)
  (space :uint16)
  (offset :long)
  (length :long)
  (buf8 :pointer))

(defcfun ("viMoveIn16" vi-move-in16)
  :long
  "Moves a block of data from the specified address space and offset to local memory."
  (vi :uint32)
  (space :uint16)
  (offset :long)
  (length :long)
  (buf16 :pointer))

(defcfun ("viMoveIn32" vi-move-in32)
  :long
  "Moves a block of data from the specified address space and offset to local memory."
  (vi :uint32)
  (space :uint16)
  (offset :long)
  (length :long)
  (buf32 :pointer))

(defcfun ("viMoveOut8" vi-move-out8)
  :long
  "Moves a block of data from local memory to the specified address space and offset."
  (vi :uint32)
  (space :uint16)
  (offset :long)
  (length :long)
  (buf8 :pointer))

(defcfun ("viMoveOut16" vi-move-out16)
  :long
  "Moves a block of data from local memory to the specified address space and offset."
  (vi :uint32)
  (space :uint16)
  (offset :long)
  (length :long)
  (buf16 :pointer))

(defcfun ("viMoveOut32" vi-move-out32)
  :long
  "Moves a block of data from local memory to the specified address space and offset."
  (vi :uint32)
  (space :uint16)
  (offset :long)
  (length :long)
  (buf32 :pointer))

(defcfun ("viOpen" vi-open)
  :long
  "Opens a session to the specified resource."
  (sesn :uint32)
  (rsrcName :string)
  (accessMode :long)
  (openTimeout :uint32)
  (vi :pointer))

(defcfun ("viOpenDefaultRM" vi-open-default-rm)
  :long
  "This function returns a session to the Default Resource Manager resource."
  (sesn :pointer))

(defcfun ("viOut8" vi-out8)
  :long
  "Writes an 8-bit, 16-bit, or 32-bit value to the specified memory space and offset."
  (vi :uint32)
  (space :uint16)
  (offset :long)
  (val8 :uint8))

(defcfun ("viOut16" vi-out16)
  :long
  "Writes an 8-bit, 16-bit, or 32-bit value to the specified memory space and offset."
  (vi :uint32)
  (space :uint16)
  (offset :long)
  (val16 :uint16))

(defcfun ("viOut32" vi-out32)
  :long
  "Writes an 8-bit, 16-bit, or 32-bit value to the specified memory space and offset."
  (vi :uint32)
  (space :uint16)
  (offset :long)
  (val32 :uint32))

(defcfun ("viParseRsrc" vi-parse-rsrc)
  :long
  "Parse a resource string to get the interface information."
  (sesn :uint32)
  (rsrcName :string)
  (intfType :pointer)
  (intfNum :pointer))

(defcfun ("viParseRsrcEx" vi-parse-rsrc-ex)
  :long
  "Parse a resource string to get extended interface information."
  (sesn :uint32)
  (rsrcName :char)
  (intfType :pointer)
  (intfNum :pointer)
  (rsrcClass :pointer)
  (expandedUnaliasedName :pointer)
  (aliasIfExists :pointer))

(defcfun ("viPeek8" vi-peek8)
  :long
  "Reads an 8-bit, 16-bit, or 32-bit value from the specified address."
  (vi :uint32)
  (addr (:pointer :void))
  (val8 :pointer))

(defcfun ("viPeek16" vi-peek16)
  :long
  "Reads an 8-bit, 16-bit, or 32-bit value from the specified address."
  (vi :uint32)
  (addr (:pointer :void))
  (val16 :pointer))

(defcfun ("viPeek32" vi-peek32)
  :long
  "Reads an 8-bit, 16-bit, or 32-bit value from the specified address."
  (vi :uint32)
  (addr (:pointer :void))
  (val32 :pointer))

(defcfun ("viPoke8" vi-poke8)
  :long
  "Writes an 8-bit, 16-bit, or 32-bit value to the specified address."
  (vi :uint32)
  (addr (:pointer :void))
  (val8 :uint8))

(defcfun ("viPoke16" vi-poke16)
  :long
  "Writes an 8-bit, 16-bit, or 32-bit value to the specified address."
  (vi :uint32)
  (addr (:pointer :void))
  (val8 :uint16))

(defcfun ("viPoke32" vi-poke32)
  :long
  "Writes an 8-bit, 16-bit, or 32-bit value to the specified address."
  (vi :uint32)
  (addr (:pointer :void))
  (val8 :uint32))

(defcfun ("viPrintf" vi-printf)
  :long
  "Converts, formats, and sends the parameters (designated by ...) to the device as specified by the format string."
  (vi :uint32)
  (writeFmt :string))

(defcfun ("viQueryf" vi-queryf)
  :long
  "Performs a formatted write and read through a single call to an operation."
  (vi :uint32)
  (writeFmt :string)
  (readFmt :string))

(defcfun ("viRead" vi-read)
  :long
  "Reads data from device or interface synchronously."
  (vi :uint32)
  (buf :pointer)
  (count :uint32)
  (retCount :pointer))

(defcfun ("viReadAsync" vi-read-async)
  :long
  "Reads data from device or interface asynchronously."
  (vi :uint32)
  (buf :pointer)
  (count :uint32)
  (jobId :pointer))

(defcfun ("viReadSTB" vi-read-stb)
  :long
  "Reads a status byte of the service request."
  (vi :uint32)
  (status :pointer))

(defcfun ("viReadToFile" vi-read-to-file)
  :long
  "Read data synchronously, and store the transferred data in a file."
  (vi :uint32)
  (fileName :string)
  (count :uint32)
  (retCount :pointer))

(defcfun ("viScanf" vi-scanf)
  :long
  "Reads, converts, and formats data using the format specifier. Stores the formatted data in the parameters (designated by ...)."
  (vi :uint32)
  (readFmt :string))

(defcfun ("viSetAttribute" vi-set-attribute)
  :long
  "Sets the state of an attribute."
  (vi :uint32)
  (attribute :uint32)
  (attrState (:pointer :void)))

(defcfun ("viSetBuf" vi-set-buf)
  :long
  "Sets the size for the formatted I/O and/or low-level I/O communication buffer(s)."
  (vi :uint32)
  (mask :uint16)
  (size :uint32))

(defcfun ("viSPrintf" vi-sprintf)
  :long
  "Converts, formats, and sends the parameters (designated by ...) to a user-specified buffer as specified by the format string."
  (vi :uint32)
  (buf :pointer)
  (writeFmt :string))

(defcfun ("viSScanf" vi-sscanf)
  :long
  "Reads, converts, and formats data from a user-specified buffer using the format specifier. Stores the formatted data in the parameters (designated by ...)."
  (vi :uint32)
  (buf :uchar)
  (readFmt :string))

(defcfun ("viStatusDesc" vi-status-desc)
  :long
  "Returns a user-readable description of the status code passed to the operation."
  (vi :uint32)
  (status :int32)
  (desc :pointer))

(defcfun ("viTerminate" vi-terminate)
  :long
  "Requests a VISA session to terminate normal execution of an operation."
  (vi :uint32)
  (degree :uint16)
  (jobId :long))

(defcfun ("viUninstallHandler" vi-uninstall-handler)
  :long
  "Uninstalls handlers for events."
  (vi :uint32)
  (eventType :long)
  (handler (:pointer :void))
  (userHandle (:pointer :void)))

(defcfun ("viUnlock" vi-unlock)
  :long
  "Relinquishes a lock for the specified resource."
  (vi :uint32))

(defcfun ("viUnmapAddress" vi-unmap-address)
  :long
  "Unmaps memory space previously mapped by viMapAddress()."
  (vi :uint32))

(defcfun ("viUnmapTrigger" vi-unmap-trigger)
  :long
  "Undo a previous map from the specified trigger source line to the specified destination line."
  (vi :uint32)
  (trigSrc :int16)
  (trigDest :int16))

(defcfun ("viUsbControlIn" vi-usb-control-in)
  :long
  "Performs a USB control pipe transfer from the device."
  (vi :uint32)
  (bmRequestType :int16)
  (bRequest :int16)
  (wValue :uint16)
  (wIndex :uint16)
  (wLength :uint16)
  (buf :pointer)
  (retCnt :pointer))

(defcfun ("viUsbControlOut" vi-usb-control-out)
  :long
  "Performs a USB control pipe transfer to the device."
  (vi :uint32)
  (bmRequestType :int16)
  (bRequest :int16)
  (wValue :uint16)
  (wIndex :uint16)
  (wLength :uint16)
  (buf :string))

(defcfun ("viVPrintf" vi-vprintf)
  :long
  "Converts, formats, and sends the parameters designated by params to the device or interface as specified by the format string."
  (vi :uint32)
  (writeFmt :string)
  (params :pointer))

(defcfun ("viVQueryf" vi-vqueryf)
  :long
  "Performs a formatted write and read through a single call to an operation."
  (vi :uint32)
  (writeFmt :string)
  (readFmt :string)
  (params :pointer))

(defcfun ("viVscanf" vi-vscanf)
  :long
  "Reads, converts, and formats data using the format specifier. Stores the formatted data in the parameters designated by params."
  (vi :uint32)
  (readFmt :string)
  (params :pointer))

(defcfun ("viVSPrintf" vi-vsprintf)
  :long
  "Converts, formats, and sends the parameters designated by params to a user-specified buffer as specified by the format string."
  (vi :uint32)
  (buf :pointer)
  (writeFmt :string)
  (params :pointer))

(defcfun ("viVSScanf" vi-vsscanf)
  :long
  "Reads, converts, and formats data from a user-specified buffer using the format specifier. Stores the formatted data in the parameters designated by params."
  (vi :uint32)
  (buf :string)
  (readFmt :string)
  (params :pointer))

(defcfun ("viVxiCommandQuery" vi-vxi-command-query)
  :long
  "Sends the device a miscellaneous command or query and/or retrieves the response to a previous query."
  (vi :uint32)
  (mode :uint16)
  (cmd :uint32)
  (response :pointer))

(defcfun ("viWaitOnEvent" vi-wait-on-event)
  :long
  "Waits for an occurrence of the specified event for a given session."
  (vi :uint32)
  (inEventType :long)
  (timeout :uint32)
  (outEventType :pointer)
  (outContext :pointer))

(defcfun ("viWrite" vi-write)
  :long
  "Writes data to device or interface synchronously."
  (vi :uint32)
  (buf :string)
  (count :uint32)
  (retCount :pointer))

(defcfun ("viWriteAsync" vi-write-async)
  :long
  "Writes data to device or interface asynchronously."
  (vi :uint32)
  (buf :string)
  (count :uint32)
  (jobId :pointer))

(defcfun ("viWriteFromFile" vi-write-from-file)
  :long
  "Take data from a file and write it out synchronously."
  (vi :uint32)
  (fileName :string)
  (count :uint32)
  (retCount :pointer))

