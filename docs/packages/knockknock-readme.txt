knockknock provides unobtrusive notifications for Emacs using posframe.
It displays temporary alert messages in a centered frame that automatically
disappears after a configurable duration.  Supports nerd-icons for beautiful
visual notifications with icons, titles, and messages.

Two layout modes are available:
- Text-based (default): Uses nerd-icons with text properties
- SVG-based: Uses SVG for pixel-perfect positioning

Usage:
  ;; Simple message (legacy API)
  (knockknock-alert "Hello, World!")
  (knockknock-alert "Custom duration" 5)

  ;; With icon and title (new API)
  (knockknock-notify :title "Build Complete"
                     :message "All tests passed!"
                     :icon "nf-cod-check"
                     :duration 5)

  ;; Enable SVG layout for pixel-perfect positioning
  (setq knockknock-use-svg-layout t)

Progress bar notifications:
  ;; Step-based progress (e.g., 3/10 tasks)
  (knockknock-progress-create :id 'build
                              :title "Building"
                              :message "Compiling..."
                              :total 10)
  (knockknock-progress-update 'build :current 3)
  (knockknock-progress-update 'build :current 10)  ; auto-closes at 100%

  ;; Percent-based progress
  (knockknock-progress-create :id 'download
                              :title "Downloading"
                              :percent 0)
  (knockknock-progress-update 'download :percent 55)
  (knockknock-progress-update 'download :percent 100)  ; auto-closes

To manually close an alert:
  M-x knockknock-close
