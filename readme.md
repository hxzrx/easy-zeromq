# soft-universal-time

Get universal time (in **MILLISECONDS**) from a variable which is periodically updated by a background thread.

This is used for very intensively time getting with not very high accuracy.

The usage is very simple:

 1. `set-software-time-resolution`, defines the updating resolution, which is default by 0.005 second.
 2. `initialize-software-time`, run the background thread to update the time.
 3. Call `*time-getter*` to get the result, in milliseconds. Here, `*time-getter*` is an function object, which is set to the real software time getter or hardware time getter, according to the time policy.

If you only want to get the OS time, just call `get-hardware-time` without the steps above.

## API

```commonlisp
;; simple usage
(set-software-time-resolution 0.01)
(initialize-software-time)
(funcall *time-getter*) ; get universal time in milliseconds

;; get an set time policy, can be either :software or :hardware
(get-time-policy) 
(set-time-policy :hardware)

;; time resolution, in seconds, should be greater than 0
(get-software-time-resolution)
(set-software-time-resolution 0.1)

(shutdown-software-time)
(restart-software-time)
(software-time-enabled-p) ; if updating thread started

;; get time directly
(get-software-time)
(get-hardware-time)
```

