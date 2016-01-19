(require 'gnome-c-align)

(defconst gnome-c-test-program-1 "\
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
")

(defconst gnome-c-test-program-1-aligned "\
GGpgCtx *g_gpg_ctx_new                   (GError              **error);

typedef void (*GGpgProgressCallback) (gpointer user_data,
                                      const gchar *what,
                                      gint type,
                                      gint current,
                                      gint total);

void     g_gpg_ctx_set_progress_callback (GGpgCtx              *ctx,
                                          GGpgProgressCallback  callback,
                                          gpointer              user_data,
                                          GDestroyNotify        destroy_data);
void     g_gpg_ctx_add_signer            (GGpgCtx              *ctx,
                                          GGpgKey              *key);
guint    g_gpg_ctx_get_n_signers         (GGpgCtx              *ctx);
GGpgKey *g_gpg_ctx_get_signer            (GGpgCtx              *ctx,
                                          guint                 index);
void     g_gpg_ctx_clear_signers         (GGpgCtx              *ctx);
")

(defconst gnome-c-test-program-2 "\
GDK_AVAILABLE_IN_3_16
const gchar **          gtk_widget_list_action_prefixes (GtkWidget             *widget);
")

(defconst gnome-c-test-program-3 "\
  /* overridable methods */
  void       (*set_property)            (GObject        *object,
                                         guint           property_id,
                                         const GValue   *value,
                                         GParamSpec     *pspec);
  void       (*get_property)            (GObject        *object,
                                         guint           property_id,
                                         GValue         *value,
                                         GParamSpec     *pspec);
")

(defconst gnome-c-test-program-4 "\
FOO_AVAILABLE_IN_ALL
int foo (struct foo ***a, int b, ...) G_GNUC_CONST;
")

(defconst gnome-c-test-program-4-aligned "\
FOO_AVAILABLE_IN_ALL
int foo (struct foo ***a,
         int           b,
         ...) G_GNUC_CONST;
")

(defconst gnome-c-test-program-5 "\
int  * bar (const char * const *  * a, int b);
")

(defconst gnome-c-test-program-5-aligned "\
int *bar (const char * const **a,
          int                  b);
")

(ert-deftest gnome-c-test-align--guess-optimal-columns ()
  "Tests the `gnome-c-align--guess-optimal-columns'."
  (with-temp-buffer
    (insert gnome-c-test-program-1)
    (c-mode)
    (let* (gnome-c-align-max-column
	   (columns
	    (gnome-c-align--guess-optimal-columns (point-min) (point-max))))
      (should (= (cdr (assq 'identifier-start-column columns)) 9))
      (should (= (cdr (assq 'arglist-start-column columns)) 41))
      (should (= (cdr (assq 'arglist-identifier-start-column columns)) 64)))))

(ert-deftest gnome-c-test-align-region ()
  "Tests the `gnome-c-align-decls-region'."
  (with-temp-buffer
    (insert gnome-c-test-program-1)
    (c-mode)
    (let (gnome-c-align-max-column)
      (gnome-c-align-guess-optimal-columns (point-min) (point-max))
      (gnome-c-align-decls-region (point-min) (point-max)))
    (should (equal (buffer-string) gnome-c-test-program-1-aligned))))

(ert-deftest gnome-c-test-align-region-2 ()
  "Tests the `gnome-c-align-decls-region'."
  (with-temp-buffer
    (insert gnome-c-test-program-4)
    (c-mode)
    (let (gnome-c-align-max-column)
      (gnome-c-align-guess-optimal-columns (point-min) (point-max))
      (gnome-c-align-decls-region (point-min) (point-max)))
    (should (equal (buffer-string) gnome-c-test-program-4-aligned))))

(ert-deftest gnome-c-test-align-region-3 ()
  "Tests the `gnome-c-align-decls-region'."
  (with-temp-buffer
    (insert gnome-c-test-program-5)
    (c-mode)
    (let (gnome-c-align-max-column)
      (gnome-c-align-guess-optimal-columns (point-min) (point-max))
      (gnome-c-align-decls-region (point-min) (point-max)))
    (should (equal (buffer-string) gnome-c-test-program-5-aligned))))

(ert-deftest gnome-c-test-align-guess-columns-1 ()
  "Tests the `gnome-c-align-guess-columns'."
  (with-temp-buffer
    (insert gnome-c-test-program-2)
    (c-mode)
    (let (gnome-c-align-max-column)
      (gnome-c-align-guess-columns (point-min) (point-max)))
    (should (= gnome-c-align-identifier-start-column 24))
    (should (= gnome-c-align-arglist-start-column 56))
    (should (= gnome-c-align-arglist-identifier-start-column 80))))

(ert-deftest gnome-c-test-align-guess-columns-2 ()
  "Tests the `gnome-c-align-guess-columns'."
  (with-temp-buffer
    (insert gnome-c-test-program-3)
    (c-mode)
    (let (gnome-c-align-max-column)
      (gnome-c-align-guess-columns (point-min) (point-max)))
    (should (= gnome-c-align-identifier-start-column 13))
    (should (= gnome-c-align-arglist-start-column 40))
    (should (= gnome-c-align-arglist-identifier-start-column 57))))
