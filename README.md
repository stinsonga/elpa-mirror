gnome-c-style
======

In the C coding style commonly used in GNOME, identifiers are written
in camel case and function arguments are aligned to the right end.
That makes it a bit cumbersome to keep your code consistent with the
style, even with align.el or plugins like yasnippet.

gnome-c-style is an Emacs minor mode intended to help editing C
source code in that style.  It mainly provides two features: text
alignment and snippet insersion.

Install
------

* Type "make"
* Copy .elc files somewhere in your load-path
* Add the following lines to ~/.emacs/init.el:

```
(autoload 'gnome-c-style-mode "gnome-c-style" "GNOME-style C minor mode" t)
(add-hook 'c-mode-hook 'gnome-c-style-mode)
```

Usage
------

| Key         | Command                                                   |
--------------|-----------------------------------------------------------|
| C-c C-g a   | Align argument list at the current point                  |
| C-c C-g r   | Align function declarations in the current region         |
| C-c C-g C-g | Compute optimal alignment columns from the current region |
| C-c C-g g   | Guess alignment columns from the current region           |
| C-c C-g s   | Set alignment column to the current point                 |
| C-c C-g c   | Insert ```module_object```                                |
| C-c C-g C   | Insert ```MODULE_OBJECT```                                |
| C-c C-g C-c | Insert ```ModuleObject```                                 |
| C-c C-g s   | Insert custom snippet                                     |

Example
------

If you have the following code in a header file:
```c
GGpgCtx *g_gpg_ctx_new (GError **error);

typedef void (*GGpgProgressCallback) (gpointer user_data,
                                      const gchar *what,
                                      gint type,
                                      gint current,
                                      gint total);

void g_gpg_ctx_set_progress_callback (GGpgCtx *ctx,
                                      GGpgProgressCallback callback,
                                      gpointer user_data,
                                      GDestroyNotify destroy_data);
void g_gpg_ctx_add_signer (GGpgCtx *ctx, GGpgKey *key);
guint g_gpg_ctx_get_n_signers (GGpgCtx *ctx);
GGpgKey *g_gpg_ctx_get_signer (GGpgCtx *ctx, guint index);
void g_gpg_ctx_clear_signers (GGpgCtx *ctx);
```

Mark the region, type ```C-c C-g C-g```, and you will see the optimum
alignment columns:

```
identifier-start: 9, arglist-start: 41, arglist-identifier-start: 63
```

Then, mark the region again, type ```C-c C-g r```, and you will get
the code aligned:

```c
GGpgCtx *g_gpg_ctx_new                   (GError             **error);

typedef void (*GGpgProgressCallback) (gpointer user_data,
                                      const gchar *what,
                                      gint type,
                                      gint current,
                                      gint total);

void     g_gpg_ctx_set_progress_callback (GGpgCtx             *ctx,
                                          GGpgProgressCallback callback,
                                          gpointer             user_data,
                                          GDestroyNotify       destroy_data);
void     g_gpg_ctx_add_signer            (GGpgCtx             *ctx,
                                          GGpgKey             *key);
guint    g_gpg_ctx_get_n_signers         (GGpgCtx             *ctx);
GGpgKey *g_gpg_ctx_get_signer            (GGpgCtx             *ctx,
                                          guint                index);
void     g_gpg_ctx_clear_signers         (GGpgCtx             *ctx);
```

Note that ```typedef``` is skipped as it is not a function declaration.
