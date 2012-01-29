{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}

module GenBigTypes where

  import GHC.Generics

  data BigSum =
            C0   |  C1  | C2   | C3   | C4   | C5   | C6   | C7   | C8   | C9
          | C10  | C11  | C12  | C13  | C14  | C15  | C16  | C17  | C18  | C19
          | C20  | C21  | C22  | C23  | C24  | C25  | C26  | C27  | C28  | C29
          | C30  | C31  | C32  | C33  | C34  | C35  | C36  | C37  | C38  | C39
          | C40  | C41  | C42  | C43  | C44  | C45  | C46  | C47  | C48  | C49
          | C50  | C51  | C52  | C53  | C54  | C55  | C56  | C57  | C58  | C59
          | C60  | C61  | C62  | C63  | C64  | C65  | C66  | C67  | C68  | C69
          | C70  | C71  | C72  | C73  | C74  | C75  | C76  | C77  | C78  | C79
          | C80  | C81  | C82  | C83  | C84  | C85  | C86  | C87  | C88  | C89
          | C90  | C91  | C92  | C93  | C94  | C95  | C96  | C97  | C98  | C99
{-
          | C100 | C101 | C102 | C103 | C104 | C105 | C106 | C107 | C108 | C109
          | C110 | C111 | C112 | C113 | C114 | C115 | C116 | C117 | C118 | C119
          | C120 | C121 | C122 | C123 | C124 | C125 | C126 | C127 | C128 | C129
          | C130 | C131 | C132 | C133 | C134 | C135 | C136 | C137 | C138 | C139
          | C140 | C141 | C142 | C143 | C144 | C145 | C146 | C147 | C148 | C149
          | C150 | C151 | C152 | C153 | C154 | C155 | C156 | C157 | C158 | C159
          | C160 | C161 | C162 | C163 | C164 | C165 | C166 | C167 | C168 | C169
          | C170 | C171 | C172 | C173 | C174 | C175 | C176 | C177 | C178 | C179
          | C180 | C181 | C182 | C183 | C184 | C185 | C186 | C187 | C188 | C189
          | C190 | C191 | C192 | C193 | C194 | C195 | C196 | C197 | C198 | C199
          | C200 | C201 | C202 | C203 | C204 | C205 | C206 | C207 | C208 | C209
          | C210 | C211 | C212 | C213 | C214 | C215 | C216 | C217 | C218 | C219
          | C220 | C221 | C222 | C223 | C224 | C225 | C226 | C227 | C228 | C229
          | C230 | C231 | C232 | C233 | C234 | C235 | C236 | C237 | C238 | C239
          | C240 | C241 | C242 | C243 | C244 | C245 | C246 | C247 | C248 | C249
          | C250 | C251 | C252 | C253 | C254 | C255 | C256 | C257 | C258 | C259
          | C260 | C261 | C262 | C263 | C264 | C265 | C266 | C267 | C268 | C269
          | C270 | C271 | C272 | C273 | C274 | C275 | C276 | C277 | C278 | C279
          | C280 | C281 | C282 | C283 | C284 | C285 | C286 | C287 | C288 | C289
          | C290 | C291 | C292 | C293 | C294 | C295 | C296 | C297 | C298 | C299
    --deriving Generic
-}

  instance Generic BigSum where
    type Rep BigSum = Rep_BigSum
    from C0 = M1 (L1 (L1 (L1 (L1 (L1 (L1 (M1 U1)))))))
    from C1 = M1 (L1 (L1 (L1 (L1 (L1 (R1 (L1 (M1 U1))))))))
    from C2 = M1 (L1 (L1 (L1 (L1 (L1 (R1 (R1 (M1 U1))))))))
    from C3 = M1 (L1 (L1 (L1 (L1 (R1 (L1 (M1 U1)))))))
    from C4 = M1 (L1 (L1 (L1 (L1 (R1 (R1 (L1 (M1 U1))))))))
    from C5 = M1 (L1 (L1 (L1 (L1 (R1 (R1 (R1 (M1 U1))))))))
    from C6 = M1 (L1 (L1 (L1 (R1 (L1 (L1 (M1 U1)))))))
    from C7 = M1 (L1 (L1 (L1 (R1 (L1 (R1 (L1 (M1 U1))))))))
    from C8 = M1 (L1 (L1 (L1 (R1 (L1 (R1 (R1 (M1 U1))))))))
    from C9 = M1 (L1 (L1 (L1 (R1 (R1 (L1 (M1 U1)))))))
    from C10 = M1 (L1 (L1 (L1 (R1 (R1 (R1 (L1 (M1 U1))))))))
    from C11 = M1 (L1 (L1 (L1 (R1 (R1 (R1 (R1 (M1 U1))))))))
    from C12 = M1 (L1 (L1 (R1 (L1 (L1 (L1 (M1 U1)))))))
    from C13 = M1 (L1 (L1 (R1 (L1 (L1 (R1 (L1 (M1 U1))))))))
    from C14 = M1 (L1 (L1 (R1 (L1 (L1 (R1 (R1 (M1 U1))))))))
    from C15 = M1 (L1 (L1 (R1 (L1 (R1 (L1 (M1 U1)))))))
    from C16 = M1 (L1 (L1 (R1 (L1 (R1 (R1 (L1 (M1 U1))))))))
    from C17 = M1 (L1 (L1 (R1 (L1 (R1 (R1 (R1 (M1 U1))))))))
    from C18 = M1 (L1 (L1 (R1 (R1 (L1 (L1 (M1 U1)))))))
    from C19 = M1 (L1 (L1 (R1 (R1 (L1 (R1 (L1 (M1 U1))))))))
    from C20 = M1 (L1 (L1 (R1 (R1 (L1 (R1 (R1 (M1 U1))))))))
    from C21 = M1 (L1 (L1 (R1 (R1 (R1 (L1 (L1 (M1 U1))))))))
    from C22 = M1 (L1 (L1 (R1 (R1 (R1 (L1 (R1 (M1 U1))))))))
    from C23 = M1 (L1 (L1 (R1 (R1 (R1 (R1 (L1 (M1 U1))))))))
    from C24 = M1 (L1 (L1 (R1 (R1 (R1 (R1 (R1 (M1 U1))))))))
    from C25 = M1 (L1 (R1 (L1 (L1 (L1 (L1 (M1 U1)))))))
    from C26 = M1 (L1 (R1 (L1 (L1 (L1 (R1 (L1 (M1 U1))))))))
    from C27 = M1 (L1 (R1 (L1 (L1 (L1 (R1 (R1 (M1 U1))))))))
    from C28 = M1 (L1 (R1 (L1 (L1 (R1 (L1 (M1 U1)))))))
    from C29 = M1 (L1 (R1 (L1 (L1 (R1 (R1 (L1 (M1 U1))))))))
    from C30 = M1 (L1 (R1 (L1 (L1 (R1 (R1 (R1 (M1 U1))))))))
    from C31 = M1 (L1 (R1 (L1 (R1 (L1 (L1 (M1 U1)))))))
    from C32 = M1 (L1 (R1 (L1 (R1 (L1 (R1 (L1 (M1 U1))))))))
    from C33 = M1 (L1 (R1 (L1 (R1 (L1 (R1 (R1 (M1 U1))))))))
    from C34 = M1 (L1 (R1 (L1 (R1 (R1 (L1 (M1 U1)))))))
    from C35 = M1 (L1 (R1 (L1 (R1 (R1 (R1 (L1 (M1 U1))))))))
    from C36 = M1 (L1 (R1 (L1 (R1 (R1 (R1 (R1 (M1 U1))))))))
    from C37 = M1 (L1 (R1 (R1 (L1 (L1 (L1 (M1 U1)))))))
    from C38 = M1 (L1 (R1 (R1 (L1 (L1 (R1 (L1 (M1 U1))))))))
    from C39 = M1 (L1 (R1 (R1 (L1 (L1 (R1 (R1 (M1 U1))))))))
    from C40 = M1 (L1 (R1 (R1 (L1 (R1 (L1 (M1 U1)))))))
    from C41 = M1 (L1 (R1 (R1 (L1 (R1 (R1 (L1 (M1 U1))))))))
    from C42 = M1 (L1 (R1 (R1 (L1 (R1 (R1 (R1 (M1 U1))))))))
    from C43 = M1 (L1 (R1 (R1 (R1 (L1 (L1 (M1 U1)))))))
    from C44 = M1 (L1 (R1 (R1 (R1 (L1 (R1 (L1 (M1 U1))))))))
    from C45 = M1 (L1 (R1 (R1 (R1 (L1 (R1 (R1 (M1 U1))))))))
    from C46 = M1 (L1 (R1 (R1 (R1 (R1 (L1 (L1 (M1 U1))))))))
    from C47 = M1 (L1 (R1 (R1 (R1 (R1 (L1 (R1 (M1 U1))))))))
    from C48 = M1 (L1 (R1 (R1 (R1 (R1 (R1 (L1 (M1 U1))))))))
    from C49 = M1 (L1 (R1 (R1 (R1 (R1 (R1 (R1 (M1 U1))))))))
    from C50 = M1 (R1 (L1 (L1 (L1 (L1 (L1 (M1 U1)))))))
    from C51 = M1 (R1 (L1 (L1 (L1 (L1 (R1 (L1 (M1 U1))))))))
    from C52 = M1 (R1 (L1 (L1 (L1 (L1 (R1 (R1 (M1 U1))))))))
    from C53 = M1 (R1 (L1 (L1 (L1 (R1 (L1 (M1 U1)))))))
    from C54 = M1 (R1 (L1 (L1 (L1 (R1 (R1 (L1 (M1 U1))))))))
    from C55 = M1 (R1 (L1 (L1 (L1 (R1 (R1 (R1 (M1 U1))))))))
    from C56 = M1 (R1 (L1 (L1 (R1 (L1 (L1 (M1 U1)))))))
    from C57 = M1 (R1 (L1 (L1 (R1 (L1 (R1 (L1 (M1 U1))))))))
    from C58 = M1 (R1 (L1 (L1 (R1 (L1 (R1 (R1 (M1 U1))))))))
    from C59 = M1 (R1 (L1 (L1 (R1 (R1 (L1 (M1 U1)))))))
    from C60 = M1 (R1 (L1 (L1 (R1 (R1 (R1 (L1 (M1 U1))))))))
    from C61 = M1 (R1 (L1 (L1 (R1 (R1 (R1 (R1 (M1 U1))))))))
    from C62 = M1 (R1 (L1 (R1 (L1 (L1 (L1 (M1 U1)))))))
    from C63 = M1 (R1 (L1 (R1 (L1 (L1 (R1 (L1 (M1 U1))))))))
    from C64 = M1 (R1 (L1 (R1 (L1 (L1 (R1 (R1 (M1 U1))))))))
    from C65 = M1 (R1 (L1 (R1 (L1 (R1 (L1 (M1 U1)))))))
    from C66 = M1 (R1 (L1 (R1 (L1 (R1 (R1 (L1 (M1 U1))))))))
    from C67 = M1 (R1 (L1 (R1 (L1 (R1 (R1 (R1 (M1 U1))))))))
    from C68 = M1 (R1 (L1 (R1 (R1 (L1 (L1 (M1 U1)))))))
    from C69 = M1 (R1 (L1 (R1 (R1 (L1 (R1 (L1 (M1 U1))))))))
    from C70 = M1 (R1 (L1 (R1 (R1 (L1 (R1 (R1 (M1 U1))))))))
    from C71 = M1 (R1 (L1 (R1 (R1 (R1 (L1 (L1 (M1 U1))))))))
    from C72 = M1 (R1 (L1 (R1 (R1 (R1 (L1 (R1 (M1 U1))))))))
    from C73 = M1 (R1 (L1 (R1 (R1 (R1 (R1 (L1 (M1 U1))))))))
    from C74 = M1 (R1 (L1 (R1 (R1 (R1 (R1 (R1 (M1 U1))))))))
    from C75 = M1 (R1 (R1 (L1 (L1 (L1 (L1 (M1 U1)))))))
    from C76 = M1 (R1 (R1 (L1 (L1 (L1 (R1 (L1 (M1 U1))))))))
    from C77 = M1 (R1 (R1 (L1 (L1 (L1 (R1 (R1 (M1 U1))))))))
    from C78 = M1 (R1 (R1 (L1 (L1 (R1 (L1 (M1 U1)))))))
    from C79 = M1 (R1 (R1 (L1 (L1 (R1 (R1 (L1 (M1 U1))))))))
    from C80 = M1 (R1 (R1 (L1 (L1 (R1 (R1 (R1 (M1 U1))))))))
    from C81 = M1 (R1 (R1 (L1 (R1 (L1 (L1 (M1 U1)))))))
    from C82 = M1 (R1 (R1 (L1 (R1 (L1 (R1 (L1 (M1 U1))))))))
    from C83 = M1 (R1 (R1 (L1 (R1 (L1 (R1 (R1 (M1 U1))))))))
    from C84 = M1 (R1 (R1 (L1 (R1 (R1 (L1 (M1 U1)))))))
    from C85 = M1 (R1 (R1 (L1 (R1 (R1 (R1 (L1 (M1 U1))))))))
    from C86 = M1 (R1 (R1 (L1 (R1 (R1 (R1 (R1 (M1 U1))))))))
    from C87 = M1 (R1 (R1 (R1 (L1 (L1 (L1 (M1 U1)))))))
    from C88 = M1 (R1 (R1 (R1 (L1 (L1 (R1 (L1 (M1 U1))))))))
    from C89 = M1 (R1 (R1 (R1 (L1 (L1 (R1 (R1 (M1 U1))))))))
    from C90 = M1 (R1 (R1 (R1 (L1 (R1 (L1 (M1 U1)))))))
    from C91 = M1 (R1 (R1 (R1 (L1 (R1 (R1 (L1 (M1 U1))))))))
    from C92 = M1 (R1 (R1 (R1 (L1 (R1 (R1 (R1 (M1 U1))))))))
    from C93 = M1 (R1 (R1 (R1 (R1 (L1 (L1 (M1 U1)))))))
    from C94 = M1 (R1 (R1 (R1 (R1 (L1 (R1 (L1 (M1 U1))))))))
    from C95 = M1 (R1 (R1 (R1 (R1 (L1 (R1 (R1 (M1 U1))))))))
    from C96 = M1 (R1 (R1 (R1 (R1 (R1 (L1 (L1 (M1 U1))))))))
    from C97 = M1 (R1 (R1 (R1 (R1 (R1 (L1 (R1 (M1 U1))))))))
    from C98 = M1 (R1 (R1 (R1 (R1 (R1 (R1 (L1 (M1 U1))))))))
    from C99 = M1 (R1 (R1 (R1 (R1 (R1 (R1 (R1 (M1 U1))))))))
    to (M1 (L1 (L1 (L1 (L1 (L1 (L1 (M1 U1)))))))) = C0
    to (M1 (L1 (L1 (L1 (L1 (L1 (R1 (L1 (M1 U1))))))))) = C1
    to (M1 (L1 (L1 (L1 (L1 (L1 (R1 (R1 (M1 U1))))))))) = C2
    to (M1 (L1 (L1 (L1 (L1 (R1 (L1 (M1 U1)))))))) = C3
    to (M1 (L1 (L1 (L1 (L1 (R1 (R1 (L1 (M1 U1))))))))) = C4
    to (M1 (L1 (L1 (L1 (L1 (R1 (R1 (R1 (M1 U1))))))))) = C5
    to (M1 (L1 (L1 (L1 (R1 (L1 (L1 (M1 U1)))))))) = C6
    to (M1 (L1 (L1 (L1 (R1 (L1 (R1 (L1 (M1 U1))))))))) = C7
    to (M1 (L1 (L1 (L1 (R1 (L1 (R1 (R1 (M1 U1))))))))) = C8
    to (M1 (L1 (L1 (L1 (R1 (R1 (L1 (M1 U1)))))))) = C9
    to (M1 (L1 (L1 (L1 (R1 (R1 (R1 (L1 (M1 U1))))))))) = C10
    to (M1 (L1 (L1 (L1 (R1 (R1 (R1 (R1 (M1 U1))))))))) = C11
    to (M1 (L1 (L1 (R1 (L1 (L1 (L1 (M1 U1)))))))) = C12
    to (M1 (L1 (L1 (R1 (L1 (L1 (R1 (L1 (M1 U1))))))))) = C13
    to (M1 (L1 (L1 (R1 (L1 (L1 (R1 (R1 (M1 U1))))))))) = C14
    to (M1 (L1 (L1 (R1 (L1 (R1 (L1 (M1 U1)))))))) = C15
    to (M1 (L1 (L1 (R1 (L1 (R1 (R1 (L1 (M1 U1))))))))) = C16
    to (M1 (L1 (L1 (R1 (L1 (R1 (R1 (R1 (M1 U1))))))))) = C17
    to (M1 (L1 (L1 (R1 (R1 (L1 (L1 (M1 U1)))))))) = C18
    to (M1 (L1 (L1 (R1 (R1 (L1 (R1 (L1 (M1 U1))))))))) = C19
    to (M1 (L1 (L1 (R1 (R1 (L1 (R1 (R1 (M1 U1))))))))) = C20
    to (M1 (L1 (L1 (R1 (R1 (R1 (L1 (L1 (M1 U1))))))))) = C21
    to (M1 (L1 (L1 (R1 (R1 (R1 (L1 (R1 (M1 U1))))))))) = C22
    to (M1 (L1 (L1 (R1 (R1 (R1 (R1 (L1 (M1 U1))))))))) = C23
    to (M1 (L1 (L1 (R1 (R1 (R1 (R1 (R1 (M1 U1))))))))) = C24
    to (M1 (L1 (R1 (L1 (L1 (L1 (L1 (M1 U1)))))))) = C25
    to (M1 (L1 (R1 (L1 (L1 (L1 (R1 (L1 (M1 U1))))))))) = C26
    to (M1 (L1 (R1 (L1 (L1 (L1 (R1 (R1 (M1 U1))))))))) = C27
    to (M1 (L1 (R1 (L1 (L1 (R1 (L1 (M1 U1)))))))) = C28
    to (M1 (L1 (R1 (L1 (L1 (R1 (R1 (L1 (M1 U1))))))))) = C29
    to (M1 (L1 (R1 (L1 (L1 (R1 (R1 (R1 (M1 U1))))))))) = C30
    to (M1 (L1 (R1 (L1 (R1 (L1 (L1 (M1 U1)))))))) = C31
    to (M1 (L1 (R1 (L1 (R1 (L1 (R1 (L1 (M1 U1))))))))) = C32
    to (M1 (L1 (R1 (L1 (R1 (L1 (R1 (R1 (M1 U1))))))))) = C33
    to (M1 (L1 (R1 (L1 (R1 (R1 (L1 (M1 U1)))))))) = C34
    to (M1 (L1 (R1 (L1 (R1 (R1 (R1 (L1 (M1 U1))))))))) = C35
    to (M1 (L1 (R1 (L1 (R1 (R1 (R1 (R1 (M1 U1))))))))) = C36
    to (M1 (L1 (R1 (R1 (L1 (L1 (L1 (M1 U1)))))))) = C37
    to (M1 (L1 (R1 (R1 (L1 (L1 (R1 (L1 (M1 U1))))))))) = C38
    to (M1 (L1 (R1 (R1 (L1 (L1 (R1 (R1 (M1 U1))))))))) = C39
    to (M1 (L1 (R1 (R1 (L1 (R1 (L1 (M1 U1)))))))) = C40
    to (M1 (L1 (R1 (R1 (L1 (R1 (R1 (L1 (M1 U1))))))))) = C41
    to (M1 (L1 (R1 (R1 (L1 (R1 (R1 (R1 (M1 U1))))))))) = C42
    to (M1 (L1 (R1 (R1 (R1 (L1 (L1 (M1 U1)))))))) = C43
    to (M1 (L1 (R1 (R1 (R1 (L1 (R1 (L1 (M1 U1))))))))) = C44
    to (M1 (L1 (R1 (R1 (R1 (L1 (R1 (R1 (M1 U1))))))))) = C45
    to (M1 (L1 (R1 (R1 (R1 (R1 (L1 (L1 (M1 U1))))))))) = C46
    to (M1 (L1 (R1 (R1 (R1 (R1 (L1 (R1 (M1 U1))))))))) = C47
    to (M1 (L1 (R1 (R1 (R1 (R1 (R1 (L1 (M1 U1))))))))) = C48
    to (M1 (L1 (R1 (R1 (R1 (R1 (R1 (R1 (M1 U1))))))))) = C49
    to (M1 (R1 (L1 (L1 (L1 (L1 (L1 (M1 U1)))))))) = C50
    to (M1 (R1 (L1 (L1 (L1 (L1 (R1 (L1 (M1 U1))))))))) = C51
    to (M1 (R1 (L1 (L1 (L1 (L1 (R1 (R1 (M1 U1))))))))) = C52
    to (M1 (R1 (L1 (L1 (L1 (R1 (L1 (M1 U1)))))))) = C53
    to (M1 (R1 (L1 (L1 (L1 (R1 (R1 (L1 (M1 U1))))))))) = C54
    to (M1 (R1 (L1 (L1 (L1 (R1 (R1 (R1 (M1 U1))))))))) = C55
    to (M1 (R1 (L1 (L1 (R1 (L1 (L1 (M1 U1)))))))) = C56
    to (M1 (R1 (L1 (L1 (R1 (L1 (R1 (L1 (M1 U1))))))))) = C57
    to (M1 (R1 (L1 (L1 (R1 (L1 (R1 (R1 (M1 U1))))))))) = C58
    to (M1 (R1 (L1 (L1 (R1 (R1 (L1 (M1 U1)))))))) = C59
    to (M1 (R1 (L1 (L1 (R1 (R1 (R1 (L1 (M1 U1))))))))) = C60
    to (M1 (R1 (L1 (L1 (R1 (R1 (R1 (R1 (M1 U1))))))))) = C61
    to (M1 (R1 (L1 (R1 (L1 (L1 (L1 (M1 U1)))))))) = C62
    to (M1 (R1 (L1 (R1 (L1 (L1 (R1 (L1 (M1 U1))))))))) = C63
    to (M1 (R1 (L1 (R1 (L1 (L1 (R1 (R1 (M1 U1))))))))) = C64
    to (M1 (R1 (L1 (R1 (L1 (R1 (L1 (M1 U1)))))))) = C65
    to (M1 (R1 (L1 (R1 (L1 (R1 (R1 (L1 (M1 U1))))))))) = C66
    to (M1 (R1 (L1 (R1 (L1 (R1 (R1 (R1 (M1 U1))))))))) = C67
    to (M1 (R1 (L1 (R1 (R1 (L1 (L1 (M1 U1)))))))) = C68
    to (M1 (R1 (L1 (R1 (R1 (L1 (R1 (L1 (M1 U1))))))))) = C69
    to (M1 (R1 (L1 (R1 (R1 (L1 (R1 (R1 (M1 U1))))))))) = C70
    to (M1 (R1 (L1 (R1 (R1 (R1 (L1 (L1 (M1 U1))))))))) = C71
    to (M1 (R1 (L1 (R1 (R1 (R1 (L1 (R1 (M1 U1))))))))) = C72
    to (M1 (R1 (L1 (R1 (R1 (R1 (R1 (L1 (M1 U1))))))))) = C73
    to (M1 (R1 (L1 (R1 (R1 (R1 (R1 (R1 (M1 U1))))))))) = C74
    to (M1 (R1 (R1 (L1 (L1 (L1 (L1 (M1 U1)))))))) = C75
    to (M1 (R1 (R1 (L1 (L1 (L1 (R1 (L1 (M1 U1))))))))) = C76
    to (M1 (R1 (R1 (L1 (L1 (L1 (R1 (R1 (M1 U1))))))))) = C77
    to (M1 (R1 (R1 (L1 (L1 (R1 (L1 (M1 U1)))))))) = C78
    to (M1 (R1 (R1 (L1 (L1 (R1 (R1 (L1 (M1 U1))))))))) = C79
    to (M1 (R1 (R1 (L1 (L1 (R1 (R1 (R1 (M1 U1))))))))) = C80
    to (M1 (R1 (R1 (L1 (R1 (L1 (L1 (M1 U1)))))))) = C81
    to (M1 (R1 (R1 (L1 (R1 (L1 (R1 (L1 (M1 U1))))))))) = C82
    to (M1 (R1 (R1 (L1 (R1 (L1 (R1 (R1 (M1 U1))))))))) = C83
    to (M1 (R1 (R1 (L1 (R1 (R1 (L1 (M1 U1)))))))) = C84
    to (M1 (R1 (R1 (L1 (R1 (R1 (R1 (L1 (M1 U1))))))))) = C85
    to (M1 (R1 (R1 (L1 (R1 (R1 (R1 (R1 (M1 U1))))))))) = C86
    to (M1 (R1 (R1 (R1 (L1 (L1 (L1 (M1 U1)))))))) = C87
    to (M1 (R1 (R1 (R1 (L1 (L1 (R1 (L1 (M1 U1))))))))) = C88
    to (M1 (R1 (R1 (R1 (L1 (L1 (R1 (R1 (M1 U1))))))))) = C89
    to (M1 (R1 (R1 (R1 (L1 (R1 (L1 (M1 U1)))))))) = C90
    to (M1 (R1 (R1 (R1 (L1 (R1 (R1 (L1 (M1 U1))))))))) = C91
    to (M1 (R1 (R1 (R1 (L1 (R1 (R1 (R1 (M1 U1))))))))) = C92
    to (M1 (R1 (R1 (R1 (R1 (L1 (L1 (M1 U1)))))))) = C93
    to (M1 (R1 (R1 (R1 (R1 (L1 (R1 (L1 (M1 U1))))))))) = C94
    to (M1 (R1 (R1 (R1 (R1 (L1 (R1 (R1 (M1 U1))))))))) = C95
    to (M1 (R1 (R1 (R1 (R1 (R1 (L1 (L1 (M1 U1))))))))) = C96
    to (M1 (R1 (R1 (R1 (R1 (R1 (L1 (R1 (M1 U1))))))))) = C97
    to (M1 (R1 (R1 (R1 (R1 (R1 (R1 (L1 (M1 U1))))))))) = C98
    to (M1 (R1 (R1 (R1 (R1 (R1 (R1 (R1 (M1 U1))))))))) = C99

  instance Datatype D1BigSum where
    datatypeName _ = "BigSum"
    moduleName _ = "GenBigTypes"

  instance Constructor C1_0BigSum where
    conName _ = "C0"

  instance Constructor C1_1BigSum where
    conName _ = "C1"

  instance Constructor C1_2BigSum where
    conName _ = "C2"

  instance Constructor C1_3BigSum where
    conName _ = "C3"

  instance Constructor C1_4BigSum where
    conName _ = "C4"

  instance Constructor C1_5BigSum where
    conName _ = "C5"

  instance Constructor C1_6BigSum where
    conName _ = "C6"

  instance Constructor C1_7BigSum where
    conName _ = "C7"

  instance Constructor C1_8BigSum where
    conName _ = "C8"

  instance Constructor C1_9BigSum where
    conName _ = "C9"

  instance Constructor C1_10BigSum where
    conName _ = "C10"

  instance Constructor C1_11BigSum where
    conName _ = "C11"

  instance Constructor C1_12BigSum where
    conName _ = "C12"

  instance Constructor C1_13BigSum where
    conName _ = "C13"

  instance Constructor C1_14BigSum where
    conName _ = "C14"

  instance Constructor C1_15BigSum where
    conName _ = "C15"

  instance Constructor C1_16BigSum where
    conName _ = "C16"

  instance Constructor C1_17BigSum where
    conName _ = "C17"

  instance Constructor C1_18BigSum where
    conName _ = "C18"

  instance Constructor C1_19BigSum where
    conName _ = "C19"

  instance Constructor C1_20BigSum where
    conName _ = "C20"

  instance Constructor C1_21BigSum where
    conName _ = "C21"

  instance Constructor C1_22BigSum where
    conName _ = "C22"

  instance Constructor C1_23BigSum where
    conName _ = "C23"

  instance Constructor C1_24BigSum where
    conName _ = "C24"

  instance Constructor C1_25BigSum where
    conName _ = "C25"

  instance Constructor C1_26BigSum where
    conName _ = "C26"

  instance Constructor C1_27BigSum where
    conName _ = "C27"

  instance Constructor C1_28BigSum where
    conName _ = "C28"

  instance Constructor C1_29BigSum where
    conName _ = "C29"

  instance Constructor C1_30BigSum where
    conName _ = "C30"

  instance Constructor C1_31BigSum where
    conName _ = "C31"

  instance Constructor C1_32BigSum where
    conName _ = "C32"

  instance Constructor C1_33BigSum where
    conName _ = "C33"

  instance Constructor C1_34BigSum where
    conName _ = "C34"

  instance Constructor C1_35BigSum where
    conName _ = "C35"

  instance Constructor C1_36BigSum where
    conName _ = "C36"

  instance Constructor C1_37BigSum where
    conName _ = "C37"

  instance Constructor C1_38BigSum where
    conName _ = "C38"

  instance Constructor C1_39BigSum where
    conName _ = "C39"

  instance Constructor C1_40BigSum where
    conName _ = "C40"

  instance Constructor C1_41BigSum where
    conName _ = "C41"

  instance Constructor C1_42BigSum where
    conName _ = "C42"

  instance Constructor C1_43BigSum where
    conName _ = "C43"

  instance Constructor C1_44BigSum where
    conName _ = "C44"

  instance Constructor C1_45BigSum where
    conName _ = "C45"

  instance Constructor C1_46BigSum where
    conName _ = "C46"

  instance Constructor C1_47BigSum where
    conName _ = "C47"

  instance Constructor C1_48BigSum where
    conName _ = "C48"

  instance Constructor C1_49BigSum where
    conName _ = "C49"

  instance Constructor C1_50BigSum where
    conName _ = "C50"

  instance Constructor C1_51BigSum where
    conName _ = "C51"

  instance Constructor C1_52BigSum where
    conName _ = "C52"

  instance Constructor C1_53BigSum where
    conName _ = "C53"

  instance Constructor C1_54BigSum where
    conName _ = "C54"

  instance Constructor C1_55BigSum where
    conName _ = "C55"

  instance Constructor C1_56BigSum where
    conName _ = "C56"

  instance Constructor C1_57BigSum where
    conName _ = "C57"

  instance Constructor C1_58BigSum where
    conName _ = "C58"

  instance Constructor C1_59BigSum where
    conName _ = "C59"

  instance Constructor C1_60BigSum where
    conName _ = "C60"

  instance Constructor C1_61BigSum where
    conName _ = "C61"

  instance Constructor C1_62BigSum where
    conName _ = "C62"

  instance Constructor C1_63BigSum where
    conName _ = "C63"

  instance Constructor C1_64BigSum where
    conName _ = "C64"

  instance Constructor C1_65BigSum where
    conName _ = "C65"

  instance Constructor C1_66BigSum where
    conName _ = "C66"

  instance Constructor C1_67BigSum where
    conName _ = "C67"

  instance Constructor C1_68BigSum where
    conName _ = "C68"

  instance Constructor C1_69BigSum where
    conName _ = "C69"

  instance Constructor C1_70BigSum where
    conName _ = "C70"

  instance Constructor C1_71BigSum where
    conName _ = "C71"

  instance Constructor C1_72BigSum where
    conName _ = "C72"

  instance Constructor C1_73BigSum where
    conName _ = "C73"

  instance Constructor C1_74BigSum where
    conName _ = "C74"

  instance Constructor C1_75BigSum where
    conName _ = "C75"

  instance Constructor C1_76BigSum where
    conName _ = "C76"

  instance Constructor C1_77BigSum where
    conName _ = "C77"

  instance Constructor C1_78BigSum where
    conName _ = "C78"

  instance Constructor C1_79BigSum where
    conName _ = "C79"

  instance Constructor C1_80BigSum where
    conName _ = "C80"

  instance Constructor C1_81BigSum where
    conName _ = "C81"

  instance Constructor C1_82BigSum where
    conName _ = "C82"

  instance Constructor C1_83BigSum where
    conName _ = "C83"

  instance Constructor C1_84BigSum where
    conName _ = "C84"

  instance Constructor C1_85BigSum where
    conName _ = "C85"

  instance Constructor C1_86BigSum where
    conName _ = "C86"

  instance Constructor C1_87BigSum where
    conName _ = "C87"

  instance Constructor C1_88BigSum where
    conName _ = "C88"

  instance Constructor C1_89BigSum where
    conName _ = "C89"

  instance Constructor C1_90BigSum where
    conName _ = "C90"

  instance Constructor C1_91BigSum where
    conName _ = "C91"

  instance Constructor C1_92BigSum where
    conName _ = "C92"

  instance Constructor C1_93BigSum where
    conName _ = "C93"

  instance Constructor C1_94BigSum where
    conName _ = "C94"

  instance Constructor C1_95BigSum where
    conName _ = "C95"

  instance Constructor C1_96BigSum where
    conName _ = "C96"

  instance Constructor C1_97BigSum where
    conName _ = "C97"

  instance Constructor C1_98BigSum where
    conName _ = "C98"

  instance Constructor C1_99BigSum where
    conName _ = "C99"

  data    D1BigSum
  data    C1_0BigSum
  data    C1_1BigSum
  data    C1_2BigSum
  data    C1_3BigSum
  data    C1_4BigSum
  data    C1_5BigSum
  data    C1_6BigSum
  data    C1_7BigSum
  data    C1_8BigSum
  data    C1_9BigSum
  data    C1_10BigSum
  data    C1_11BigSum
  data    C1_12BigSum
  data    C1_13BigSum
  data    C1_14BigSum
  data    C1_15BigSum
  data    C1_16BigSum
  data    C1_17BigSum
  data    C1_18BigSum
  data    C1_19BigSum
  data    C1_20BigSum
  data    C1_21BigSum
  data    C1_22BigSum
  data    C1_23BigSum
  data    C1_24BigSum
  data    C1_25BigSum
  data    C1_26BigSum
  data    C1_27BigSum
  data    C1_28BigSum
  data    C1_29BigSum
  data    C1_30BigSum
  data    C1_31BigSum
  data    C1_32BigSum
  data    C1_33BigSum
  data    C1_34BigSum
  data    C1_35BigSum
  data    C1_36BigSum
  data    C1_37BigSum
  data    C1_38BigSum
  data    C1_39BigSum
  data    C1_40BigSum
  data    C1_41BigSum
  data    C1_42BigSum
  data    C1_43BigSum
  data    C1_44BigSum
  data    C1_45BigSum
  data    C1_46BigSum
  data    C1_47BigSum
  data    C1_48BigSum
  data    C1_49BigSum
  data    C1_50BigSum
  data    C1_51BigSum
  data    C1_52BigSum
  data    C1_53BigSum
  data    C1_54BigSum
  data    C1_55BigSum
  data    C1_56BigSum
  data    C1_57BigSum
  data    C1_58BigSum
  data    C1_59BigSum
  data    C1_60BigSum
  data    C1_61BigSum
  data    C1_62BigSum
  data    C1_63BigSum
  data    C1_64BigSum
  data    C1_65BigSum
  data    C1_66BigSum
  data    C1_67BigSum
  data    C1_68BigSum
  data    C1_69BigSum
  data    C1_70BigSum
  data    C1_71BigSum
  data    C1_72BigSum
  data    C1_73BigSum
  data    C1_74BigSum
  data    C1_75BigSum
  data    C1_76BigSum
  data    C1_77BigSum
  data    C1_78BigSum
  data    C1_79BigSum
  data    C1_80BigSum
  data    C1_81BigSum
  data    C1_82BigSum
  data    C1_83BigSum
  data    C1_84BigSum
  data    C1_85BigSum
  data    C1_86BigSum
  data    C1_87BigSum
  data    C1_88BigSum
  data    C1_89BigSum
  data    C1_90BigSum
  data    C1_91BigSum
  data    C1_92BigSum
  data    C1_93BigSum
  data    C1_94BigSum
  data    C1_95BigSum
  data    C1_96BigSum
  data    C1_97BigSum
  data    C1_98BigSum
  data    C1_99BigSum

  type Rep_BigSum = D1
                               D1BigSum
                               ((((((C1 C1_0BigSum U1
                                     :+: (C1
                                                         C1_1BigSum U1
                                                       :+: C1
                                                                          C1_2BigSum
                                                                          U1))
                                    :+: (C1
                                                        C1_3BigSum U1
                                                      :+: (C1
                                                                          C1_4BigSum
                                                                          U1
                                                                        :+: C1
                                                                                           C1_5BigSum
                                                                                           U1)))
                                   :+: ((C1
                                                        C1_6BigSum U1
                                                      :+: (C1
                                                                          C1_7BigSum
                                                                          U1
                                                                        :+: C1
                                                                                           C1_8BigSum
                                                                                           U1))
                                                     :+: (C1
                                                                         C1_9BigSum
                                                                         U1
                                                                       :+: (C1
                                                                                           C1_10BigSum
                                                                                           U1
                                                                                         :+: C1
                                                                                                            C1_11BigSum
                                                                                                            U1))))
                                  :+: (((C1
                                                        C1_12BigSum U1
                                                      :+: (C1
                                                                          C1_13BigSum
                                                                          U1
                                                                        :+: C1
                                                                                           C1_14BigSum
                                                                                           U1))
                                                     :+: (C1
                                                                         C1_15BigSum
                                                                         U1
                                                                       :+: (C1
                                                                                           C1_16BigSum
                                                                                           U1
                                                                                         :+: C1
                                                                                                            C1_17BigSum
                                                                                                            U1)))
                                                    :+: ((C1
                                                                         C1_18BigSum
                                                                         U1
                                                                       :+: (C1
                                                                                           C1_19BigSum
                                                                                           U1
                                                                                         :+: C1
                                                                                                            C1_20BigSum
                                                                                                            U1))
                                                                      :+: ((C1
                                                                                           C1_21BigSum
                                                                                           U1
                                                                                         :+: C1
                                                                                                            C1_22BigSum
                                                                                                            U1)
                                                                                        :+: (C1
                                                                                                            C1_23BigSum
                                                                                                            U1
                                                                                                          :+: C1
                                                                                                                             C1_24BigSum
                                                                                                                             U1)))))
                                 :+: ((((C1
                                                        C1_25BigSum U1
                                                      :+: (C1
                                                                          C1_26BigSum
                                                                          U1
                                                                        :+: C1
                                                                                           C1_27BigSum
                                                                                           U1))
                                                     :+: (C1
                                                                         C1_28BigSum
                                                                         U1
                                                                       :+: (C1
                                                                                           C1_29BigSum
                                                                                           U1
                                                                                         :+: C1
                                                                                                            C1_30BigSum
                                                                                                            U1)))
                                                    :+: ((C1
                                                                         C1_31BigSum
                                                                         U1
                                                                       :+: (C1
                                                                                           C1_32BigSum
                                                                                           U1
                                                                                         :+: C1
                                                                                                            C1_33BigSum
                                                                                                            U1))
                                                                      :+: (C1
                                                                                          C1_34BigSum
                                                                                          U1
                                                                                        :+: (C1
                                                                                                            C1_35BigSum
                                                                                                            U1
                                                                                                          :+: C1
                                                                                                                             C1_36BigSum
                                                                                                                             U1))))
                                                   :+: (((C1
                                                                         C1_37BigSum
                                                                         U1
                                                                       :+: (C1
                                                                                           C1_38BigSum
                                                                                           U1
                                                                                         :+: C1
                                                                                                            C1_39BigSum
                                                                                                            U1))
                                                                      :+: (C1
                                                                                          C1_40BigSum
                                                                                          U1
                                                                                        :+: (C1
                                                                                                            C1_41BigSum
                                                                                                            U1
                                                                                                          :+: C1
                                                                                                                             C1_42BigSum
                                                                                                                             U1)))
                                                                     :+: ((C1
                                                                                          C1_43BigSum
                                                                                          U1
                                                                                        :+: (C1
                                                                                                            C1_44BigSum
                                                                                                            U1
                                                                                                          :+: C1
                                                                                                                             C1_45BigSum
                                                                                                                             U1))
                                                                                       :+: ((C1
                                                                                                            C1_46BigSum
                                                                                                            U1
                                                                                                          :+: C1
                                                                                                                             C1_47BigSum
                                                                                                                             U1)
                                                                                                         :+: (C1
                                                                                                                             C1_48BigSum
                                                                                                                             U1
                                                                                                                           :+: C1
                                                                                                                                              C1_49BigSum
                                                                                                                                              U1))))))
                                :+: (((((C1
                                                        C1_50BigSum U1
                                                      :+: (C1
                                                                          C1_51BigSum
                                                                          U1
                                                                        :+: C1
                                                                                           C1_52BigSum
                                                                                           U1))
                                                     :+: (C1
                                                                         C1_53BigSum
                                                                         U1
                                                                       :+: (C1
                                                                                           C1_54BigSum
                                                                                           U1
                                                                                         :+: C1
                                                                                                            C1_55BigSum
                                                                                                            U1)))
                                                    :+: ((C1
                                                                         C1_56BigSum
                                                                         U1
                                                                       :+: (C1
                                                                                           C1_57BigSum
                                                                                           U1
                                                                                         :+: C1
                                                                                                            C1_58BigSum
                                                                                                            U1))
                                                                      :+: (C1
                                                                                          C1_59BigSum
                                                                                          U1
                                                                                        :+: (C1
                                                                                                            C1_60BigSum
                                                                                                            U1
                                                                                                          :+: C1
                                                                                                                             C1_61BigSum
                                                                                                                             U1))))
                                                   :+: (((C1
                                                                         C1_62BigSum
                                                                         U1
                                                                       :+: (C1
                                                                                           C1_63BigSum
                                                                                           U1
                                                                                         :+: C1
                                                                                                            C1_64BigSum
                                                                                                            U1))
                                                                      :+: (C1
                                                                                          C1_65BigSum
                                                                                          U1
                                                                                        :+: (C1
                                                                                                            C1_66BigSum
                                                                                                            U1
                                                                                                          :+: C1
                                                                                                                             C1_67BigSum
                                                                                                                             U1)))
                                                                     :+: ((C1
                                                                                          C1_68BigSum
                                                                                          U1
                                                                                        :+: (C1
                                                                                                            C1_69BigSum
                                                                                                            U1
                                                                                                          :+: C1
                                                                                                                             C1_70BigSum
                                                                                                                             U1))
                                                                                       :+: ((C1
                                                                                                            C1_71BigSum
                                                                                                            U1
                                                                                                          :+: C1
                                                                                                                             C1_72BigSum
                                                                                                                             U1)
                                                                                                         :+: (C1
                                                                                                                             C1_73BigSum
                                                                                                                             U1
                                                                                                                           :+: C1
                                                                                                                                              C1_74BigSum
                                                                                                                                              U1)))))
                                                  :+: ((((C1
                                                                         C1_75BigSum
                                                                         U1
                                                                       :+: (C1
                                                                                           C1_76BigSum
                                                                                           U1
                                                                                         :+: C1
                                                                                                            C1_77BigSum
                                                                                                            U1))
                                                                      :+: (C1
                                                                                          C1_78BigSum
                                                                                          U1
                                                                                        :+: (C1
                                                                                                            C1_79BigSum
                                                                                                            U1
                                                                                                          :+: C1
                                                                                                                             C1_80BigSum
                                                                                                                             U1)))
                                                                     :+: ((C1
                                                                                          C1_81BigSum
                                                                                          U1
                                                                                        :+: (C1
                                                                                                            C1_82BigSum
                                                                                                            U1
                                                                                                          :+: C1
                                                                                                                             C1_83BigSum
                                                                                                                             U1))
                                                                                       :+: (C1
                                                                                                           C1_84BigSum
                                                                                                           U1
                                                                                                         :+: (C1
                                                                                                                             C1_85BigSum
                                                                                                                             U1
                                                                                                                           :+: C1
                                                                                                                                              C1_86BigSum
                                                                                                                                              U1))))
                                                                    :+: (((C1
                                                                                          C1_87BigSum
                                                                                          U1
                                                                                        :+: (C1
                                                                                                            C1_88BigSum
                                                                                                            U1
                                                                                                          :+: C1
                                                                                                                             C1_89BigSum
                                                                                                                             U1))
                                                                                       :+: (C1
                                                                                                           C1_90BigSum
                                                                                                           U1
                                                                                                         :+: (C1
                                                                                                                             C1_91BigSum
                                                                                                                             U1
                                                                                                                           :+: C1
                                                                                                                                              C1_92BigSum
                                                                                                                                              U1)))
                                                                                      :+: ((C1
                                                                                                           C1_93BigSum
                                                                                                           U1
                                                                                                         :+: (C1
                                                                                                                             C1_94BigSum
                                                                                                                             U1
                                                                                                                           :+: C1
                                                                                                                                              C1_95BigSum
                                                                                                                                              U1))
                                                                                                        :+: ((C1
                                                                                                                             C1_96BigSum
                                                                                                                             U1
                                                                                                                           :+: C1
                                                                                                                                              C1_97BigSum
                                                                                                                                              U1)
                                                                                                                          :+: (C1
                                                                                                                                              C1_98BigSum
                                                                                                                                              U1
                                                                                                                                            :+: C1
                                                                                                                                                               C1_99BigSum
                                                                                                                                                               U1)))))))





{-
data BigProduct = C
    () () () () () () () () () ()
    () () () () () () () () () ()
    () () () () () () () () () ()
    () () () () () () () () () ()
    () () () () () () () () () ()
    () () () () () () () () () ()
    () () () () () () () () () ()
    () () () () () () () () () ()
    () () () () () () () () () ()
    () () () () () () () () () ()
    () () () () () () () () () ()
    () () () () () () () () () ()
    () () () () () () () () () ()
    () () () () () () () () () ()
    () () () () () () () () () ()
    () () () () () () () () () ()
    () () () () () () () () () ()
    () () () () () () () () () ()
    () () () () () () () () () ()
    () () () () () () () () () ()
    () () () () () () () () () ()
    () () () () () () () () () ()
    () () () () () () () () () ()
    () () () () () () () () () ()
    () () () () () () () () () ()
    () () () () () () () () () ()
    () () () () () () () () () ()
    () () () () () () () () () ()
    () () () () () () () () () ()
    () () () () () () () () () ()
    deriving Generic
-}
