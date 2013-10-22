(defvar *ew-wunderground-api-key* "d4654918a43f7c9e"
  "wunderground.com developer api key, free version can handle up
  to 500 requests per day.")
(defvar *ew-weather-location* "27707"
  "String containing the zip-code or \"State/City\" where `State'
  is the two-letter state abbreviation, and `City' is a valid
  wunderground city in that state (ie \"NY/New York\") from
  whence to draw weather information.")
(defvar *ew-auto-ip-lookup* t
  "If non-nil use wunderground's auto-ip api to return weather
  information")
(defun ew-get-time (search-string)
  "Retrieve time from json field according to `search-string'"
  (let ((buf (current-buffer)))
    (save-excursion
      (switch-to-buffer 
       (url-retrieve-synchronously (concat "http://api.wunderground.com/api/" 
					   *ew-wunderground-api-key*
					   "/astronomy/q/" 
					   *ew-weather-location* ".json")))
      (search-backward "sunrise" nil t))))

(defun ew-get-sunrise ()
  (interactive)
  )

