# Cl-Visa32 - The Visa32 Binding for Common Lisp
##                  You can instrument control in our favorite Common Lisp.

## Usage

```common-lisp
(defvar *session* (cffi:foreign-alloc :uint32))
(defvar *vi-session* (cffi:foreign-alloc :uint32))
(defvar *return-count* (cffi:foreign-alloc :uint32))
(defvar *gpib-address* "GPIB::21::INSTR")

(visa32:vi-open-default-rm *session*)

(visa32:vi-open *session* *gpib-address* 0 0 *vi-session*)

(visa32::vi-write *vi-session* "*RST" (length "*RST") *return-count*)
```

## Installation

```bash
cd $HOME/quicklisp/local-projects
git clone https://github.com/a-nano/cl-visa32.git
```
Then, use quicklisp to install the libraries required by cl-visa32:

Start your lisp. Then, just:

```common-lisp
(ql:quickload :cl-visa32)
```
Put the visa32.dll to "/cl-visa32/lib/".

## Author

* Akihide Nano (an74abc@gmail.com)

## Copyright

Copyright (c) 2016 Akihide Nano (an74abc@gmail.com)

## License

Licensed under the LLGPL License.
