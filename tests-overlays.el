To: philcm@gnu.org
From: "Philippe C." <xaccrocheur@gmail.com>
Date: Thu, 2 Aug 2012 05:07:51 +0000
Subject: pdf
Return-Path: <xaccrocheur@gmail.com>
Delivered-To: GMX delivery to philcm@gmx.com
Received: (qmail invoked by alias); 02 Aug 2012 05:07:54 -0000
Received: from fencepost.gnu.org (EHLO fencepost.gnu.org) [208.118.235.10]
  by mx0.gmx.com (mx-us005) with SMTP; 02 Aug 2012 01:07:54 -0400
Received: from eggs.gnu.org ([208.118.235.92]:41566)
	by fencepost.gnu.org with esmtps (TLS1.0:RSA_AES_256_CBC_SHA1:32)
	(Exim 4.71)
	(envelope-from <xaccrocheur@gmail.com>)
	id 1SwndY-0007Uu-SD
	for philcm@gnu.org; Thu, 02 Aug 2012 01:07:52 -0400
Received: from Debian-exim by eggs.gnu.org with spam-scanned (Exim 4.71)
	(envelope-from <xaccrocheur@gmail.com>)
	id 1SwndY-0006e4-1V
	for philcm@gnu.org; Thu, 02 Aug 2012 01:07:52 -0400
X-Spam-Checker-Version: SpamAssassin 3.3.2 (2011-06-06) on eggs.gnu.org
X-Spam-Status: No, score=-2.6 required=5.0 tests=BAYES_00,FREEMAIL_FROM,
	HTML_MESSAGE,RCVD_IN_DNSWL_LOW,T_DKIM_INVALID,T_FREEMAIL_DOC_PDF
	autolearn=ham version=3.3.2
Received: from mail-ob0-f169.google.com ([209.85.214.169]:50668)
	by eggs.gnu.org with esmtp (Exim 4.71)
	(envelope-from <xaccrocheur@gmail.com>)
	id 1SwndX-0006dv-Qe
	for philcm@gnu.org; Thu, 02 Aug 2012 01:07:51 -0400
Received: by obhx4 with SMTP id x4so16001344obh.0
        for <philcm@gnu.org>; Wed, 01 Aug 2012 22:07:51 -0700 (PDT)
DKIM-Signature: v=1; a=rsa-sha256; c=relaxed/relaxed;
        d=gmail.com; s=20120113;
        h=mime-version:sender:x-google-sender-delegation:date
         :x-google-sender-auth:message-id:subject:from:to:content-type;
        bh=6HPR5uZUrMMvXcHjhgsEgsr1UsMSlADOy0aj8R7uZ+A=;
        b=Ea81oSb/CSDb5pBWjLOCBNQU6mWJA2DpYTmKT5vdyLlHliYGJ/I5A2JPXwGxsGibuH
         uxpRqbu7OnWZF8wBs/8lZbqDzD6r8lqeyWHzH2UdQ5gKBtKPiCSbWrUCnDZLfdTxatUz
         ZcQrYo7lXQep2Iogod1FofmazU8veGz+qEJavzD89LQkbRnPVx7gnk13GNIn5FmWGVFL
         ze0G0X0ASTivxVW2BYqLPEAgBvC4kgQRRzg44zwhwisFCalt7NKF0231CzP2ie1sQCbu
         y40dQ2e2b4NDYOQS3+0jXIfUF+GA8LxO6L5I7jJOua4kBURxXAqzLnSJi3eeXRZ9Bk0k
         /vGw==
MIME-Version: 1.0
Received: by 10.182.50.68 with SMTP id a4mr33196811obo.59.1343884071228; Wed,
 01 Aug 2012 22:07:51 -0700 (PDT)
Sender: philippe.coatmeur@gmail.com
X-Google-Sender-Delegation: philippe.coatmeur@gmail.com
Received: by 10.76.11.169 with HTTP; Wed, 1 Aug 2012 22:07:51 -0700 (PDT)
X-Google-Sender-Auth: NVdlbAtvShDjfT_LUDYGZwQmyMQ
Message-ID: <CAC9=7GU2wWtptEKDd=ZeK9RC1ud+4psTAEG8fcY4Mxn1RDsscA@mail.gmail.com>
Content-Type: multipart/mixed; boundary=f46d04446bbdaf54a104c6416473
X-detected-operating-system: by eggs.gnu.org: Genre and OS details not recognized.
X-Received-From: 209.85.214.169
X-GMX-Antivirus: 0 (no virus found)
X-GMX-Antispam: 0 (Mail was not recognized as spam);
 Detail=5D7Q89H36p4L00VTXC6D4q0N+AH0PUCnGL2vqOgpaBYL16oitsMrgPmd5OTScXXTbWtmO
 Vg30EBFxfFA0LE89362fqADenNflI0Qtx3y2v8N97LGdZDSssHxepDySK/u16nJymL/X+irLsoEZ
 S88sdtjUpVrjexnnnMUysOO++pNY4xTm1cJSA==V1;


--text follows this line--


---Decoded--
--f46d04446bbdaf549d04c6416471
Content-Type: text/plain; charset=UTF-8

pdf now!

--
Phil CM

--f46d04446bbdaf549d04c6416471
Content-Type: text/html; charset=UTF-8

<div dir="ltr"><br clear="all">pdf now!<br><br>-- <br><div dir="ltr">Phil CM</div><br>
</div>

--f46d04446bbdaf549d04c6416471--

---body fini--


--attachment links follows this line--

Attached:alternative[1]	Attached:TEXT/plain[1.1]	Attached:TEXT/html[1.2]
Attached:'gnus-logo.pdf' {APPLICATION/pdf}[2]


(defun my-imapua-toggle-headers ()
  "Toggle headers."
  (interactive)
  (beginning-of-buffer)
  (message "point is %s" (point))
  ;; (widen)
  ;; (save-excursion
  (search-forward-regexp "Subject")
  ;; (toggle-read-only)
  (end-of-line)
	(setq buffer-read-only nil)
  ;; (newline 1)
  (next-line)
  (beginning-of-line)
  ;; (insert "ha! -- ")
  ;; (next-line 1)
	(push-mark)
  (forward-paragraph)
  (message "function imapua-hide-headers: %s"
           (imapua-hide-headers))
  ;; (hide-region-hide)
  ;; (right-char)
  (deactivate-mark)
    ;; (toggle-read-only)
	;; (setq buffer-read-only 't)
	;; (set-buffer-modified-p nil)
	)

(if (car hide-region-overlays)
		(message "hidden")
	(message "shown"))


;; pX:
(defun my-px-imapua-toggle-headers ()
  "Toggle headers."
  (interactive)
  (if (car hide-region-overlays)
      (progn
        (message "shown")
        (hide-region-unhide))
    (progn
      (message "hidden")
      (beginning-of-buffer)
      (message "point is %s" (point))
      (search-forward-regexp "Subject")
      ;; (toggle-read-only)
      (end-of-line)
      (setq buffer-read-only nil)
      (next-line)
      (beginning-of-line)
      (push-mark)
      (forward-paragraph)
      (imapua-hide-headers)
      (deactivate-mark)
      ;; (toggle-read-only)
      ;; (setq buffer-read-only 't)
      ;; (set-buffer-modified-p nil)
      )))
