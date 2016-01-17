(require 'gobject-align)

(defconst gobject-test-program-1 "\
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

(defconst gobject-test-program-1-aligned "\
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
")

(defconst gobject-test-program-2 "\
GDK_AVAILABLE_IN_3_16
const gchar **          gtk_widget_list_action_prefixes (GtkWidget             *widget);
")

(defconst gobject-test-program-3 "\
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

(ert-deftest gobject-test-align--compute-optimal-columns ()
  "Tests the `gobject-align--compute-optimal-columns'."
  (with-temp-buffer
    (insert gobject-test-program-1)
    (c-mode)
    (let ((columns (gobject-align--compute-optimal-columns (point-min) (point-max))))
      (should (= (cdr (assq 'identifier-start-column columns)) 9))
      (should (= (cdr (assq 'arglist-start-column columns)) 41))
      (should (= (cdr (assq 'arglist-identifier-start-column columns)) 63)))))

(ert-deftest gobject-test-align-region ()
  "Tests the `gobject-align-region'."
  (with-temp-buffer
    (insert gobject-test-program-1)
    (c-mode)
    (gobject-align-compute-optimal-columns (point-min) (point-max))
    (gobject-align-region (point-min) (point-max))
    (should (equal (buffer-string) gobject-test-program-1-aligned))))

(ert-deftest gobject-test-align-guess-columns-1 ()
  "Tests the `gobject-align-guess-columns'."
  (with-temp-buffer
    (insert gobject-test-program-2)
    (c-mode)
    (gobject-align-guess-columns (point-min) (point-max))
    (should (= gobject-align-identifier-start-column 24))
    (should (= gobject-align-arglist-start-column 56))
    (should (= gobject-align-arglist-identifier-start-column 80))))

(ert-deftest gobject-test-align-guess-columns-2 ()
  "Tests the `gobject-align-guess-columns'."
  (with-temp-buffer
    (insert gobject-test-program-3)
    (c-mode)
    (gobject-align-guess-columns (point-min) (point-max))
    (should (= gobject-align-identifier-start-column 13))
    (should (= gobject-align-arglist-start-column 40))
    (should (= gobject-align-arglist-identifier-start-column 57))))
