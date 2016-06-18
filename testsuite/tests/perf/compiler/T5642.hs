{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
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
    from x = M1 (case x of
      C0  -> L1 (L1 (L1 (L1 (L1 (L1 (M1 U1))))))
      C1  -> L1 (L1 (L1 (L1 (L1 (R1 (L1 (M1 U1)))))))
      C2  -> L1 (L1 (L1 (L1 (L1 (R1 (R1 (M1 U1)))))))
      C3  -> L1 (L1 (L1 (L1 (R1 (L1 (M1 U1))))))
      C4  -> L1 (L1 (L1 (L1 (R1 (R1 (L1 (M1 U1)))))))
      C5  -> L1 (L1 (L1 (L1 (R1 (R1 (R1 (M1 U1)))))))
      C6  -> L1 (L1 (L1 (R1 (L1 (L1 (M1 U1))))))
      C7  -> L1 (L1 (L1 (R1 (L1 (R1 (L1 (M1 U1)))))))
      C8  -> L1 (L1 (L1 (R1 (L1 (R1 (R1 (M1 U1)))))))
      C9  -> L1 (L1 (L1 (R1 (R1 (L1 (M1 U1))))))
      C10 -> L1 (L1 (L1 (R1 (R1 (R1 (L1 (M1 U1)))))))
      C11 -> L1 (L1 (L1 (R1 (R1 (R1 (R1 (M1 U1)))))))
      C12 -> L1 (L1 (R1 (L1 (L1 (L1 (M1 U1))))))
      C13 -> L1 (L1 (R1 (L1 (L1 (R1 (L1 (M1 U1)))))))
      C14 -> L1 (L1 (R1 (L1 (L1 (R1 (R1 (M1 U1)))))))
      C15 -> L1 (L1 (R1 (L1 (R1 (L1 (M1 U1))))))
      C16 -> L1 (L1 (R1 (L1 (R1 (R1 (L1 (M1 U1)))))))
      C17 -> L1 (L1 (R1 (L1 (R1 (R1 (R1 (M1 U1)))))))
      C18 -> L1 (L1 (R1 (R1 (L1 (L1 (M1 U1))))))
      C19 -> L1 (L1 (R1 (R1 (L1 (R1 (L1 (M1 U1)))))))
      C20 -> L1 (L1 (R1 (R1 (L1 (R1 (R1 (M1 U1)))))))
      C21 -> L1 (L1 (R1 (R1 (R1 (L1 (L1 (M1 U1)))))))
      C22 -> L1 (L1 (R1 (R1 (R1 (L1 (R1 (M1 U1)))))))
      C23 -> L1 (L1 (R1 (R1 (R1 (R1 (L1 (M1 U1)))))))
      C24 -> L1 (L1 (R1 (R1 (R1 (R1 (R1 (M1 U1)))))))
      C25 -> L1 (R1 (L1 (L1 (L1 (L1 (M1 U1))))))
      C26 -> L1 (R1 (L1 (L1 (L1 (R1 (L1 (M1 U1)))))))
      C27 -> L1 (R1 (L1 (L1 (L1 (R1 (R1 (M1 U1)))))))
      C28 -> L1 (R1 (L1 (L1 (R1 (L1 (M1 U1))))))
      C29 -> L1 (R1 (L1 (L1 (R1 (R1 (L1 (M1 U1)))))))
      C30 -> L1 (R1 (L1 (L1 (R1 (R1 (R1 (M1 U1)))))))
      C31 -> L1 (R1 (L1 (R1 (L1 (L1 (M1 U1))))))
      C32 -> L1 (R1 (L1 (R1 (L1 (R1 (L1 (M1 U1)))))))
      C33 -> L1 (R1 (L1 (R1 (L1 (R1 (R1 (M1 U1)))))))
      C34 -> L1 (R1 (L1 (R1 (R1 (L1 (M1 U1))))))
      C35 -> L1 (R1 (L1 (R1 (R1 (R1 (L1 (M1 U1)))))))
      C36 -> L1 (R1 (L1 (R1 (R1 (R1 (R1 (M1 U1)))))))
      C37 -> L1 (R1 (R1 (L1 (L1 (L1 (M1 U1))))))
      C38 -> L1 (R1 (R1 (L1 (L1 (R1 (L1 (M1 U1)))))))
      C39 -> L1 (R1 (R1 (L1 (L1 (R1 (R1 (M1 U1)))))))
      C40 -> L1 (R1 (R1 (L1 (R1 (L1 (M1 U1))))))
      C41 -> L1 (R1 (R1 (L1 (R1 (R1 (L1 (M1 U1)))))))
      C42 -> L1 (R1 (R1 (L1 (R1 (R1 (R1 (M1 U1)))))))
      C43 -> L1 (R1 (R1 (R1 (L1 (L1 (M1 U1))))))
      C44 -> L1 (R1 (R1 (R1 (L1 (R1 (L1 (M1 U1)))))))
      C45 -> L1 (R1 (R1 (R1 (L1 (R1 (R1 (M1 U1)))))))
      C46 -> L1 (R1 (R1 (R1 (R1 (L1 (L1 (M1 U1)))))))
      C47 -> L1 (R1 (R1 (R1 (R1 (L1 (R1 (M1 U1)))))))
      C48 -> L1 (R1 (R1 (R1 (R1 (R1 (L1 (M1 U1)))))))
      C49 -> L1 (R1 (R1 (R1 (R1 (R1 (R1 (M1 U1)))))))
      C50 -> R1 (L1 (L1 (L1 (L1 (L1 (M1 U1))))))
      C51 -> R1 (L1 (L1 (L1 (L1 (R1 (L1 (M1 U1)))))))
      C52 -> R1 (L1 (L1 (L1 (L1 (R1 (R1 (M1 U1)))))))
      C53 -> R1 (L1 (L1 (L1 (R1 (L1 (M1 U1))))))
      C54 -> R1 (L1 (L1 (L1 (R1 (R1 (L1 (M1 U1)))))))
      C55 -> R1 (L1 (L1 (L1 (R1 (R1 (R1 (M1 U1)))))))
      C56 -> R1 (L1 (L1 (R1 (L1 (L1 (M1 U1))))))
      C57 -> R1 (L1 (L1 (R1 (L1 (R1 (L1 (M1 U1)))))))
      C58 -> R1 (L1 (L1 (R1 (L1 (R1 (R1 (M1 U1)))))))
      C59 -> R1 (L1 (L1 (R1 (R1 (L1 (M1 U1))))))
      C60 -> R1 (L1 (L1 (R1 (R1 (R1 (L1 (M1 U1)))))))
      C61 -> R1 (L1 (L1 (R1 (R1 (R1 (R1 (M1 U1)))))))
      C62 -> R1 (L1 (R1 (L1 (L1 (L1 (M1 U1))))))
      C63 -> R1 (L1 (R1 (L1 (L1 (R1 (L1 (M1 U1)))))))
      C64 -> R1 (L1 (R1 (L1 (L1 (R1 (R1 (M1 U1)))))))
      C65 -> R1 (L1 (R1 (L1 (R1 (L1 (M1 U1))))))
      C66 -> R1 (L1 (R1 (L1 (R1 (R1 (L1 (M1 U1)))))))
      C67 -> R1 (L1 (R1 (L1 (R1 (R1 (R1 (M1 U1)))))))
      C68 -> R1 (L1 (R1 (R1 (L1 (L1 (M1 U1))))))
      C69 -> R1 (L1 (R1 (R1 (L1 (R1 (L1 (M1 U1)))))))
      C70 -> R1 (L1 (R1 (R1 (L1 (R1 (R1 (M1 U1)))))))
      C71 -> R1 (L1 (R1 (R1 (R1 (L1 (L1 (M1 U1)))))))
      C72 -> R1 (L1 (R1 (R1 (R1 (L1 (R1 (M1 U1)))))))
      C73 -> R1 (L1 (R1 (R1 (R1 (R1 (L1 (M1 U1)))))))
      C74 -> R1 (L1 (R1 (R1 (R1 (R1 (R1 (M1 U1)))))))
      C75 -> R1 (R1 (L1 (L1 (L1 (L1 (M1 U1))))))
      C76 -> R1 (R1 (L1 (L1 (L1 (R1 (L1 (M1 U1)))))))
      C77 -> R1 (R1 (L1 (L1 (L1 (R1 (R1 (M1 U1)))))))
      C78 -> R1 (R1 (L1 (L1 (R1 (L1 (M1 U1))))))
      C79 -> R1 (R1 (L1 (L1 (R1 (R1 (L1 (M1 U1)))))))
      C80 -> R1 (R1 (L1 (L1 (R1 (R1 (R1 (M1 U1)))))))
      C81 -> R1 (R1 (L1 (R1 (L1 (L1 (M1 U1))))))
      C82 -> R1 (R1 (L1 (R1 (L1 (R1 (L1 (M1 U1)))))))
      C83 -> R1 (R1 (L1 (R1 (L1 (R1 (R1 (M1 U1)))))))
      C84 -> R1 (R1 (L1 (R1 (R1 (L1 (M1 U1))))))
      C85 -> R1 (R1 (L1 (R1 (R1 (R1 (L1 (M1 U1)))))))
      C86 -> R1 (R1 (L1 (R1 (R1 (R1 (R1 (M1 U1)))))))
      C87 -> R1 (R1 (R1 (L1 (L1 (L1 (M1 U1))))))
      C88 -> R1 (R1 (R1 (L1 (L1 (R1 (L1 (M1 U1)))))))
      C89 -> R1 (R1 (R1 (L1 (L1 (R1 (R1 (M1 U1)))))))
      C90 -> R1 (R1 (R1 (L1 (R1 (L1 (M1 U1))))))
      C91 -> R1 (R1 (R1 (L1 (R1 (R1 (L1 (M1 U1)))))))
      C92 -> R1 (R1 (R1 (L1 (R1 (R1 (R1 (M1 U1)))))))
      C93 -> R1 (R1 (R1 (R1 (L1 (L1 (M1 U1))))))
      C94 -> R1 (R1 (R1 (R1 (L1 (R1 (L1 (M1 U1)))))))
      C95 -> R1 (R1 (R1 (R1 (L1 (R1 (R1 (M1 U1)))))))
      C96 -> R1 (R1 (R1 (R1 (R1 (L1 (L1 (M1 U1)))))))
      C97 -> R1 (R1 (R1 (R1 (R1 (L1 (R1 (M1 U1)))))))
      C98 -> R1 (R1 (R1 (R1 (R1 (R1 (L1 (M1 U1)))))))
      C99 -> R1 (R1 (R1 (R1 (R1 (R1 (R1 (M1 U1))))))))
    to (M1 x) = case x of
      L1 (L1 (L1 (L1 (L1 (L1 (M1 U1)))))) -> C0
      L1 (L1 (L1 (L1 (L1 (R1 (L1 (M1 U1))))))) -> C1
      L1 (L1 (L1 (L1 (L1 (R1 (R1 (M1 U1))))))) -> C2
      L1 (L1 (L1 (L1 (R1 (L1 (M1 U1)))))) -> C3
      L1 (L1 (L1 (L1 (R1 (R1 (L1 (M1 U1))))))) -> C4
      L1 (L1 (L1 (L1 (R1 (R1 (R1 (M1 U1))))))) -> C5
      L1 (L1 (L1 (R1 (L1 (L1 (M1 U1)))))) -> C6
      L1 (L1 (L1 (R1 (L1 (R1 (L1 (M1 U1))))))) -> C7
      L1 (L1 (L1 (R1 (L1 (R1 (R1 (M1 U1))))))) -> C8
      L1 (L1 (L1 (R1 (R1 (L1 (M1 U1)))))) -> C9
      L1 (L1 (L1 (R1 (R1 (R1 (L1 (M1 U1))))))) -> C10
      L1 (L1 (L1 (R1 (R1 (R1 (R1 (M1 U1))))))) -> C11
      L1 (L1 (R1 (L1 (L1 (L1 (M1 U1)))))) -> C12
      L1 (L1 (R1 (L1 (L1 (R1 (L1 (M1 U1))))))) -> C13
      L1 (L1 (R1 (L1 (L1 (R1 (R1 (M1 U1))))))) -> C14
      L1 (L1 (R1 (L1 (R1 (L1 (M1 U1)))))) -> C15
      L1 (L1 (R1 (L1 (R1 (R1 (L1 (M1 U1))))))) -> C16
      L1 (L1 (R1 (L1 (R1 (R1 (R1 (M1 U1))))))) -> C17
      L1 (L1 (R1 (R1 (L1 (L1 (M1 U1)))))) -> C18
      L1 (L1 (R1 (R1 (L1 (R1 (L1 (M1 U1))))))) -> C19
      L1 (L1 (R1 (R1 (L1 (R1 (R1 (M1 U1))))))) -> C20
      L1 (L1 (R1 (R1 (R1 (L1 (L1 (M1 U1))))))) -> C21
      L1 (L1 (R1 (R1 (R1 (L1 (R1 (M1 U1))))))) -> C22
      L1 (L1 (R1 (R1 (R1 (R1 (L1 (M1 U1))))))) -> C23
      L1 (L1 (R1 (R1 (R1 (R1 (R1 (M1 U1))))))) -> C24
      L1 (R1 (L1 (L1 (L1 (L1 (M1 U1)))))) -> C25
      L1 (R1 (L1 (L1 (L1 (R1 (L1 (M1 U1))))))) -> C26
      L1 (R1 (L1 (L1 (L1 (R1 (R1 (M1 U1))))))) -> C27
      L1 (R1 (L1 (L1 (R1 (L1 (M1 U1)))))) -> C28
      L1 (R1 (L1 (L1 (R1 (R1 (L1 (M1 U1))))))) -> C29
      L1 (R1 (L1 (L1 (R1 (R1 (R1 (M1 U1))))))) -> C30
      L1 (R1 (L1 (R1 (L1 (L1 (M1 U1)))))) -> C31
      L1 (R1 (L1 (R1 (L1 (R1 (L1 (M1 U1))))))) -> C32
      L1 (R1 (L1 (R1 (L1 (R1 (R1 (M1 U1))))))) -> C33
      L1 (R1 (L1 (R1 (R1 (L1 (M1 U1)))))) -> C34
      L1 (R1 (L1 (R1 (R1 (R1 (L1 (M1 U1))))))) -> C35
      L1 (R1 (L1 (R1 (R1 (R1 (R1 (M1 U1))))))) -> C36
      L1 (R1 (R1 (L1 (L1 (L1 (M1 U1)))))) -> C37
      L1 (R1 (R1 (L1 (L1 (R1 (L1 (M1 U1))))))) -> C38
      L1 (R1 (R1 (L1 (L1 (R1 (R1 (M1 U1))))))) -> C39
      L1 (R1 (R1 (L1 (R1 (L1 (M1 U1)))))) -> C40
      L1 (R1 (R1 (L1 (R1 (R1 (L1 (M1 U1))))))) -> C41
      L1 (R1 (R1 (L1 (R1 (R1 (R1 (M1 U1))))))) -> C42
      L1 (R1 (R1 (R1 (L1 (L1 (M1 U1)))))) -> C43
      L1 (R1 (R1 (R1 (L1 (R1 (L1 (M1 U1))))))) -> C44
      L1 (R1 (R1 (R1 (L1 (R1 (R1 (M1 U1))))))) -> C45
      L1 (R1 (R1 (R1 (R1 (L1 (L1 (M1 U1))))))) -> C46
      L1 (R1 (R1 (R1 (R1 (L1 (R1 (M1 U1))))))) -> C47
      L1 (R1 (R1 (R1 (R1 (R1 (L1 (M1 U1))))))) -> C48
      L1 (R1 (R1 (R1 (R1 (R1 (R1 (M1 U1))))))) -> C49
      R1 (L1 (L1 (L1 (L1 (L1 (M1 U1)))))) -> C50
      R1 (L1 (L1 (L1 (L1 (R1 (L1 (M1 U1))))))) -> C51
      R1 (L1 (L1 (L1 (L1 (R1 (R1 (M1 U1))))))) -> C52
      R1 (L1 (L1 (L1 (R1 (L1 (M1 U1)))))) -> C53
      R1 (L1 (L1 (L1 (R1 (R1 (L1 (M1 U1))))))) -> C54
      R1 (L1 (L1 (L1 (R1 (R1 (R1 (M1 U1))))))) -> C55
      R1 (L1 (L1 (R1 (L1 (L1 (M1 U1)))))) -> C56
      R1 (L1 (L1 (R1 (L1 (R1 (L1 (M1 U1))))))) -> C57
      R1 (L1 (L1 (R1 (L1 (R1 (R1 (M1 U1))))))) -> C58
      R1 (L1 (L1 (R1 (R1 (L1 (M1 U1)))))) -> C59
      R1 (L1 (L1 (R1 (R1 (R1 (L1 (M1 U1))))))) -> C60
      R1 (L1 (L1 (R1 (R1 (R1 (R1 (M1 U1))))))) -> C61
      R1 (L1 (R1 (L1 (L1 (L1 (M1 U1)))))) -> C62
      R1 (L1 (R1 (L1 (L1 (R1 (L1 (M1 U1))))))) -> C63
      R1 (L1 (R1 (L1 (L1 (R1 (R1 (M1 U1))))))) -> C64
      R1 (L1 (R1 (L1 (R1 (L1 (M1 U1)))))) -> C65
      R1 (L1 (R1 (L1 (R1 (R1 (L1 (M1 U1))))))) -> C66
      R1 (L1 (R1 (L1 (R1 (R1 (R1 (M1 U1))))))) -> C67
      R1 (L1 (R1 (R1 (L1 (L1 (M1 U1)))))) -> C68
      R1 (L1 (R1 (R1 (L1 (R1 (L1 (M1 U1))))))) -> C69
      R1 (L1 (R1 (R1 (L1 (R1 (R1 (M1 U1))))))) -> C70
      R1 (L1 (R1 (R1 (R1 (L1 (L1 (M1 U1))))))) -> C71
      R1 (L1 (R1 (R1 (R1 (L1 (R1 (M1 U1))))))) -> C72
      R1 (L1 (R1 (R1 (R1 (R1 (L1 (M1 U1))))))) -> C73
      R1 (L1 (R1 (R1 (R1 (R1 (R1 (M1 U1))))))) -> C74
      R1 (R1 (L1 (L1 (L1 (L1 (M1 U1)))))) -> C75
      R1 (R1 (L1 (L1 (L1 (R1 (L1 (M1 U1))))))) -> C76
      R1 (R1 (L1 (L1 (L1 (R1 (R1 (M1 U1))))))) -> C77
      R1 (R1 (L1 (L1 (R1 (L1 (M1 U1)))))) -> C78
      R1 (R1 (L1 (L1 (R1 (R1 (L1 (M1 U1))))))) -> C79
      R1 (R1 (L1 (L1 (R1 (R1 (R1 (M1 U1))))))) -> C80
      R1 (R1 (L1 (R1 (L1 (L1 (M1 U1)))))) -> C81
      R1 (R1 (L1 (R1 (L1 (R1 (L1 (M1 U1))))))) -> C82
      R1 (R1 (L1 (R1 (L1 (R1 (R1 (M1 U1))))))) -> C83
      R1 (R1 (L1 (R1 (R1 (L1 (M1 U1)))))) -> C84
      R1 (R1 (L1 (R1 (R1 (R1 (L1 (M1 U1))))))) -> C85
      R1 (R1 (L1 (R1 (R1 (R1 (R1 (M1 U1))))))) -> C86
      R1 (R1 (R1 (L1 (L1 (L1 (M1 U1)))))) -> C87
      R1 (R1 (R1 (L1 (L1 (R1 (L1 (M1 U1))))))) -> C88
      R1 (R1 (R1 (L1 (L1 (R1 (R1 (M1 U1))))))) -> C89
      R1 (R1 (R1 (L1 (R1 (L1 (M1 U1)))))) -> C90
      R1 (R1 (R1 (L1 (R1 (R1 (L1 (M1 U1))))))) -> C91
      R1 (R1 (R1 (L1 (R1 (R1 (R1 (M1 U1))))))) -> C92
      R1 (R1 (R1 (R1 (L1 (L1 (M1 U1)))))) -> C93
      R1 (R1 (R1 (R1 (L1 (R1 (L1 (M1 U1))))))) -> C94
      R1 (R1 (R1 (R1 (L1 (R1 (R1 (M1 U1))))))) -> C95
      R1 (R1 (R1 (R1 (R1 (L1 (L1 (M1 U1))))))) -> C96
      R1 (R1 (R1 (R1 (R1 (L1 (R1 (M1 U1))))))) -> C97
      R1 (R1 (R1 (R1 (R1 (R1 (L1 (M1 U1))))))) -> C98
      R1 (R1 (R1 (R1 (R1 (R1 (R1 (M1 U1))))))) -> C99

  type Rep_BigSum = D1
                                       ('MetaData
                                          "BigSum" "Wat" "main" 'False)
                                       ((((((C1
                                               ('MetaCons
                                                  "C0" 'PrefixI 'False)
                                               U1
                                             :+: (C1
                                                                 ('MetaCons
                                                                    "C1"
                                                                    'PrefixI
                                                                    'False)
                                                                 U1
                                                               :+: C1
                                                                                  ('MetaCons
                                                                                     "C2"
                                                                                     'PrefixI
                                                                                     'False)
                                                                                  U1))
                                            :+: (C1
                                                                ('MetaCons
                                                                   "C3"
                                                                   'PrefixI
                                                                   'False)
                                                                U1
                                                              :+: (C1
                                                                                  ('MetaCons
                                                                                     "C4"
                                                                                     'PrefixI
                                                                                     'False)
                                                                                  U1
                                                                                :+: C1
                                                                                                   ('MetaCons
                                                                                                      "C5"
                                                                                                      'PrefixI
                                                                                                      'False)
                                                                                                   U1)))
                                           :+: ((C1
                                                                ('MetaCons
                                                                   "C6"
                                                                   'PrefixI
                                                                   'False)
                                                                U1
                                                              :+: (C1
                                                                                  ('MetaCons
                                                                                     "C7"
                                                                                     'PrefixI
                                                                                     'False)
                                                                                  U1
                                                                                :+: C1
                                                                                                   ('MetaCons
                                                                                                      "C8"
                                                                                                      'PrefixI
                                                                                                      'False)
                                                                                                   U1))
                                                             :+: (C1
                                                                                 ('MetaCons
                                                                                    "C9"
                                                                                    'PrefixI
                                                                                    'False)
                                                                                 U1
                                                                               :+: (C1
                                                                                                   ('MetaCons
                                                                                                      "C10"
                                                                                                      'PrefixI
                                                                                                      'False)
                                                                                                   U1
                                                                                                 :+: C1
                                                                                                                    ('MetaCons
                                                                                                                       "C11"
                                                                                                                       'PrefixI
                                                                                                                       'False)
                                                                                                                    U1))))
                                          :+: (((C1
                                                                ('MetaCons
                                                                   "C12"
                                                                   'PrefixI
                                                                   'False)
                                                                U1
                                                              :+: (C1
                                                                                  ('MetaCons
                                                                                     "C13"
                                                                                     'PrefixI
                                                                                     'False)
                                                                                  U1
                                                                                :+: C1
                                                                                                   ('MetaCons
                                                                                                      "C14"
                                                                                                      'PrefixI
                                                                                                      'False)
                                                                                                   U1))
                                                             :+: (C1
                                                                                 ('MetaCons
                                                                                    "C15"
                                                                                    'PrefixI
                                                                                    'False)
                                                                                 U1
                                                                               :+: (C1
                                                                                                   ('MetaCons
                                                                                                      "C16"
                                                                                                      'PrefixI
                                                                                                      'False)
                                                                                                   U1
                                                                                                 :+: C1
                                                                                                                    ('MetaCons
                                                                                                                       "C17"
                                                                                                                       'PrefixI
                                                                                                                       'False)
                                                                                                                    U1)))
                                                            :+: ((C1
                                                                                 ('MetaCons
                                                                                    "C18"
                                                                                    'PrefixI
                                                                                    'False)
                                                                                 U1
                                                                               :+: (C1
                                                                                                   ('MetaCons
                                                                                                      "C19"
                                                                                                      'PrefixI
                                                                                                      'False)
                                                                                                   U1
                                                                                                 :+: C1
                                                                                                                    ('MetaCons
                                                                                                                       "C20"
                                                                                                                       'PrefixI
                                                                                                                       'False)
                                                                                                                    U1))
                                                                              :+: ((C1
                                                                                                   ('MetaCons
                                                                                                      "C21"
                                                                                                      'PrefixI
                                                                                                      'False)
                                                                                                   U1
                                                                                                 :+: C1
                                                                                                                    ('MetaCons
                                                                                                                       "C22"
                                                                                                                       'PrefixI
                                                                                                                       'False)
                                                                                                                    U1)
                                                                                                :+: (C1
                                                                                                                    ('MetaCons
                                                                                                                       "C23"
                                                                                                                       'PrefixI
                                                                                                                       'False)
                                                                                                                    U1
                                                                                                                  :+: C1
                                                                                                                                     ('MetaCons
                                                                                                                                        "C24"
                                                                                                                                        'PrefixI
                                                                                                                                        'False)
                                                                                                                                     U1)))))
                                         :+: ((((C1
                                                                ('MetaCons
                                                                   "C25"
                                                                   'PrefixI
                                                                   'False)
                                                                U1
                                                              :+: (C1
                                                                                  ('MetaCons
                                                                                     "C26"
                                                                                     'PrefixI
                                                                                     'False)
                                                                                  U1
                                                                                :+: C1
                                                                                                   ('MetaCons
                                                                                                      "C27"
                                                                                                      'PrefixI
                                                                                                      'False)
                                                                                                   U1))
                                                             :+: (C1
                                                                                 ('MetaCons
                                                                                    "C28"
                                                                                    'PrefixI
                                                                                    'False)
                                                                                 U1
                                                                               :+: (C1
                                                                                                   ('MetaCons
                                                                                                      "C29"
                                                                                                      'PrefixI
                                                                                                      'False)
                                                                                                   U1
                                                                                                 :+: C1
                                                                                                                    ('MetaCons
                                                                                                                       "C30"
                                                                                                                       'PrefixI
                                                                                                                       'False)
                                                                                                                    U1)))
                                                            :+: ((C1
                                                                                 ('MetaCons
                                                                                    "C31"
                                                                                    'PrefixI
                                                                                    'False)
                                                                                 U1
                                                                               :+: (C1
                                                                                                   ('MetaCons
                                                                                                      "C32"
                                                                                                      'PrefixI
                                                                                                      'False)
                                                                                                   U1
                                                                                                 :+: C1
                                                                                                                    ('MetaCons
                                                                                                                       "C33"
                                                                                                                       'PrefixI
                                                                                                                       'False)
                                                                                                                    U1))
                                                                              :+: (C1
                                                                                                  ('MetaCons
                                                                                                     "C34"
                                                                                                     'PrefixI
                                                                                                     'False)
                                                                                                  U1
                                                                                                :+: (C1
                                                                                                                    ('MetaCons
                                                                                                                       "C35"
                                                                                                                       'PrefixI
                                                                                                                       'False)
                                                                                                                    U1
                                                                                                                  :+: C1
                                                                                                                                     ('MetaCons
                                                                                                                                        "C36"
                                                                                                                                        'PrefixI
                                                                                                                                        'False)
                                                                                                                                     U1))))
                                                           :+: (((C1
                                                                                 ('MetaCons
                                                                                    "C37"
                                                                                    'PrefixI
                                                                                    'False)
                                                                                 U1
                                                                               :+: (C1
                                                                                                   ('MetaCons
                                                                                                      "C38"
                                                                                                      'PrefixI
                                                                                                      'False)
                                                                                                   U1
                                                                                                 :+: C1
                                                                                                                    ('MetaCons
                                                                                                                       "C39"
                                                                                                                       'PrefixI
                                                                                                                       'False)
                                                                                                                    U1))
                                                                              :+: (C1
                                                                                                  ('MetaCons
                                                                                                     "C40"
                                                                                                     'PrefixI
                                                                                                     'False)
                                                                                                  U1
                                                                                                :+: (C1
                                                                                                                    ('MetaCons
                                                                                                                       "C41"
                                                                                                                       'PrefixI
                                                                                                                       'False)
                                                                                                                    U1
                                                                                                                  :+: C1
                                                                                                                                     ('MetaCons
                                                                                                                                        "C42"
                                                                                                                                        'PrefixI
                                                                                                                                        'False)
                                                                                                                                     U1)))
                                                                             :+: ((C1
                                                                                                  ('MetaCons
                                                                                                     "C43"
                                                                                                     'PrefixI
                                                                                                     'False)
                                                                                                  U1
                                                                                                :+: (C1
                                                                                                                    ('MetaCons
                                                                                                                       "C44"
                                                                                                                       'PrefixI
                                                                                                                       'False)
                                                                                                                    U1
                                                                                                                  :+: C1
                                                                                                                                     ('MetaCons
                                                                                                                                        "C45"
                                                                                                                                        'PrefixI
                                                                                                                                        'False)
                                                                                                                                     U1))
                                                                                               :+: ((C1
                                                                                                                    ('MetaCons
                                                                                                                       "C46"
                                                                                                                       'PrefixI
                                                                                                                       'False)
                                                                                                                    U1
                                                                                                                  :+: C1
                                                                                                                                     ('MetaCons
                                                                                                                                        "C47"
                                                                                                                                        'PrefixI
                                                                                                                                        'False)
                                                                                                                                     U1)
                                                                                                                 :+: (C1
                                                                                                                                     ('MetaCons
                                                                                                                                        "C48"
                                                                                                                                        'PrefixI
                                                                                                                                        'False)
                                                                                                                                     U1
                                                                                                                                   :+: C1
                                                                                                                                                      ('MetaCons
                                                                                                                                                         "C49"
                                                                                                                                                         'PrefixI
                                                                                                                                                         'False)
                                                                                                                                                      U1))))))
                                        :+: (((((C1
                                                                ('MetaCons
                                                                   "C50"
                                                                   'PrefixI
                                                                   'False)
                                                                U1
                                                              :+: (C1
                                                                                  ('MetaCons
                                                                                     "C51"
                                                                                     'PrefixI
                                                                                     'False)
                                                                                  U1
                                                                                :+: C1
                                                                                                   ('MetaCons
                                                                                                      "C52"
                                                                                                      'PrefixI
                                                                                                      'False)
                                                                                                   U1))
                                                             :+: (C1
                                                                                 ('MetaCons
                                                                                    "C53"
                                                                                    'PrefixI
                                                                                    'False)
                                                                                 U1
                                                                               :+: (C1
                                                                                                   ('MetaCons
                                                                                                      "C54"
                                                                                                      'PrefixI
                                                                                                      'False)
                                                                                                   U1
                                                                                                 :+: C1
                                                                                                                    ('MetaCons
                                                                                                                       "C55"
                                                                                                                       'PrefixI
                                                                                                                       'False)
                                                                                                                    U1)))
                                                            :+: ((C1
                                                                                 ('MetaCons
                                                                                    "C56"
                                                                                    'PrefixI
                                                                                    'False)
                                                                                 U1
                                                                               :+: (C1
                                                                                                   ('MetaCons
                                                                                                      "C57"
                                                                                                      'PrefixI
                                                                                                      'False)
                                                                                                   U1
                                                                                                 :+: C1
                                                                                                                    ('MetaCons
                                                                                                                       "C58"
                                                                                                                       'PrefixI
                                                                                                                       'False)
                                                                                                                    U1))
                                                                              :+: (C1
                                                                                                  ('MetaCons
                                                                                                     "C59"
                                                                                                     'PrefixI
                                                                                                     'False)
                                                                                                  U1
                                                                                                :+: (C1
                                                                                                                    ('MetaCons
                                                                                                                       "C60"
                                                                                                                       'PrefixI
                                                                                                                       'False)
                                                                                                                    U1
                                                                                                                  :+: C1
                                                                                                                                     ('MetaCons
                                                                                                                                        "C61"
                                                                                                                                        'PrefixI
                                                                                                                                        'False)
                                                                                                                                     U1))))
                                                           :+: (((C1
                                                                                 ('MetaCons
                                                                                    "C62"
                                                                                    'PrefixI
                                                                                    'False)
                                                                                 U1
                                                                               :+: (C1
                                                                                                   ('MetaCons
                                                                                                      "C63"
                                                                                                      'PrefixI
                                                                                                      'False)
                                                                                                   U1
                                                                                                 :+: C1
                                                                                                                    ('MetaCons
                                                                                                                       "C64"
                                                                                                                       'PrefixI
                                                                                                                       'False)
                                                                                                                    U1))
                                                                              :+: (C1
                                                                                                  ('MetaCons
                                                                                                     "C65"
                                                                                                     'PrefixI
                                                                                                     'False)
                                                                                                  U1
                                                                                                :+: (C1
                                                                                                                    ('MetaCons
                                                                                                                       "C66"
                                                                                                                       'PrefixI
                                                                                                                       'False)
                                                                                                                    U1
                                                                                                                  :+: C1
                                                                                                                                     ('MetaCons
                                                                                                                                        "C67"
                                                                                                                                        'PrefixI
                                                                                                                                        'False)
                                                                                                                                     U1)))
                                                                             :+: ((C1
                                                                                                  ('MetaCons
                                                                                                     "C68"
                                                                                                     'PrefixI
                                                                                                     'False)
                                                                                                  U1
                                                                                                :+: (C1
                                                                                                                    ('MetaCons
                                                                                                                       "C69"
                                                                                                                       'PrefixI
                                                                                                                       'False)
                                                                                                                    U1
                                                                                                                  :+: C1
                                                                                                                                     ('MetaCons
                                                                                                                                        "C70"
                                                                                                                                        'PrefixI
                                                                                                                                        'False)
                                                                                                                                     U1))
                                                                                               :+: ((C1
                                                                                                                    ('MetaCons
                                                                                                                       "C71"
                                                                                                                       'PrefixI
                                                                                                                       'False)
                                                                                                                    U1
                                                                                                                  :+: C1
                                                                                                                                     ('MetaCons
                                                                                                                                        "C72"
                                                                                                                                        'PrefixI
                                                                                                                                        'False)
                                                                                                                                     U1)
                                                                                                                 :+: (C1
                                                                                                                                     ('MetaCons
                                                                                                                                        "C73"
                                                                                                                                        'PrefixI
                                                                                                                                        'False)
                                                                                                                                     U1
                                                                                                                                   :+: C1
                                                                                                                                                      ('MetaCons
                                                                                                                                                         "C74"
                                                                                                                                                         'PrefixI
                                                                                                                                                         'False)
                                                                                                                                                      U1)))))
                                                          :+: ((((C1
                                                                                 ('MetaCons
                                                                                    "C75"
                                                                                    'PrefixI
                                                                                    'False)
                                                                                 U1
                                                                               :+: (C1
                                                                                                   ('MetaCons
                                                                                                      "C76"
                                                                                                      'PrefixI
                                                                                                      'False)
                                                                                                   U1
                                                                                                 :+: C1
                                                                                                                    ('MetaCons
                                                                                                                       "C77"
                                                                                                                       'PrefixI
                                                                                                                       'False)
                                                                                                                    U1))
                                                                              :+: (C1
                                                                                                  ('MetaCons
                                                                                                     "C78"
                                                                                                     'PrefixI
                                                                                                     'False)
                                                                                                  U1
                                                                                                :+: (C1
                                                                                                                    ('MetaCons
                                                                                                                       "C79"
                                                                                                                       'PrefixI
                                                                                                                       'False)
                                                                                                                    U1
                                                                                                                  :+: C1
                                                                                                                                     ('MetaCons
                                                                                                                                        "C80"
                                                                                                                                        'PrefixI
                                                                                                                                        'False)
                                                                                                                                     U1)))
                                                                             :+: ((C1
                                                                                                  ('MetaCons
                                                                                                     "C81"
                                                                                                     'PrefixI
                                                                                                     'False)
                                                                                                  U1
                                                                                                :+: (C1
                                                                                                                    ('MetaCons
                                                                                                                       "C82"
                                                                                                                       'PrefixI
                                                                                                                       'False)
                                                                                                                    U1
                                                                                                                  :+: C1
                                                                                                                                     ('MetaCons
                                                                                                                                        "C83"
                                                                                                                                        'PrefixI
                                                                                                                                        'False)
                                                                                                                                     U1))
                                                                                               :+: (C1
                                                                                                                   ('MetaCons
                                                                                                                      "C84"
                                                                                                                      'PrefixI
                                                                                                                      'False)
                                                                                                                   U1
                                                                                                                 :+: (C1
                                                                                                                                     ('MetaCons
                                                                                                                                        "C85"
                                                                                                                                        'PrefixI
                                                                                                                                        'False)
                                                                                                                                     U1
                                                                                                                                   :+: C1
                                                                                                                                                      ('MetaCons
                                                                                                                                                         "C86"
                                                                                                                                                         'PrefixI
                                                                                                                                                         'False)
                                                                                                                                                      U1))))
                                                                            :+: (((C1
                                                                                                  ('MetaCons
                                                                                                     "C87"
                                                                                                     'PrefixI
                                                                                                     'False)
                                                                                                  U1
                                                                                                :+: (C1
                                                                                                                    ('MetaCons
                                                                                                                       "C88"
                                                                                                                       'PrefixI
                                                                                                                       'False)
                                                                                                                    U1
                                                                                                                  :+: C1
                                                                                                                                     ('MetaCons
                                                                                                                                        "C89"
                                                                                                                                        'PrefixI
                                                                                                                                        'False)
                                                                                                                                     U1))
                                                                                               :+: (C1
                                                                                                                   ('MetaCons
                                                                                                                      "C90"
                                                                                                                      'PrefixI
                                                                                                                      'False)
                                                                                                                   U1
                                                                                                                 :+: (C1
                                                                                                                                     ('MetaCons
                                                                                                                                        "C91"
                                                                                                                                        'PrefixI
                                                                                                                                        'False)
                                                                                                                                     U1
                                                                                                                                   :+: C1
                                                                                                                                                      ('MetaCons
                                                                                                                                                         "C92"
                                                                                                                                                         'PrefixI
                                                                                                                                                         'False)
                                                                                                                                                      U1)))
                                                                                              :+: ((C1
                                                                                                                   ('MetaCons
                                                                                                                      "C93"
                                                                                                                      'PrefixI
                                                                                                                      'False)
                                                                                                                   U1
                                                                                                                 :+: (C1
                                                                                                                                     ('MetaCons
                                                                                                                                        "C94"
                                                                                                                                        'PrefixI
                                                                                                                                        'False)
                                                                                                                                     U1
                                                                                                                                   :+: C1
                                                                                                                                                      ('MetaCons
                                                                                                                                                         "C95"
                                                                                                                                                         'PrefixI
                                                                                                                                                         'False)
                                                                                                                                                      U1))
                                                                                                                :+: ((C1
                                                                                                                                     ('MetaCons
                                                                                                                                        "C96"
                                                                                                                                        'PrefixI
                                                                                                                                        'False)
                                                                                                                                     U1
                                                                                                                                   :+: C1
                                                                                                                                                      ('MetaCons
                                                                                                                                                         "C97"
                                                                                                                                                         'PrefixI
                                                                                                                                                         'False)
                                                                                                                                                      U1)
                                                                                                                                  :+: (C1
                                                                                                                                                      ('MetaCons
                                                                                                                                                         "C98"
                                                                                                                                                         'PrefixI
                                                                                                                                                         'False)
                                                                                                                                                      U1
                                                                                                                                                    :+: C1
                                                                                                                                                                       ('MetaCons
                                                                                                                                                                          "C99"
                                                                                                                                                                          'PrefixI
                                                                                                                                                                          'False)
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
