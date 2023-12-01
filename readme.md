# soft-universal-time

Get universal time (in **MILLISECONDS**) from a variable which is periodically updated by a background thread.

This is used for very intensively time getting with not very high accuracy.

The usage is very simple:

 1. `set-soft-time-resolution`, defines the updating resolution, which is default by 0.005 second.
 2. `initialize-soft-time`, run the background thread to update the time.
 3. Call `*time-getter*` to get the result, in milliseconds. Here, `*time-getter*` is an function object, which is set to the real soft time getter or hard time getter, according to the time policy.

If you only want to get the OS time, just call `get-hard-time` without the steps above.

## API

```commonlisp
;; simple usage
(set-soft-time-resolution 0.01)
(initialize-soft-time)
(funcall *time-getter*) ; get universal time in milliseconds

;; get an set time policy, can be either :soft or :hard, default :soft
(get-time-policy)
(set-time-policy :hard)

;; time resolution, in seconds, should be greater than 0
(get-soft-time-resolution)
(set-soft-time-resolution 0.1)

(shutdown-soft-time)
(restart-soft-time)
(soft-time-enabled-p) ; if updating thread started

;; get time directly
(get-soft-time)
(get-hard-time)
```
