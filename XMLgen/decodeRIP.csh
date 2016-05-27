#
#  set decodeRIP = "r1-3-4-6i1p2"

# initialize to blank
   set physics = ""
   set suffix = ""
   set propertyName = ""
   set propertyValue = ""

   set physics = `echo $decodeRIP|awk '{ print substr(  $0, length($0), length($0) ) }'`

# separate the realization information from the RIP
   set Realz=`echo $decodeRIP|awk '{ print substr(  $0, 1, length($0) - 4 ) }'`

# check for ensemble or individual realization
   if (("$Realz" =~ *to*) || ("$Realz" =~ *-*)) then
      set suffix = "_ensmbl"
      set propertyName = "RIP"
      set outRIP = '$(RIP)'
      if ("$Realz" =~ *to*) then
        set ensBeg=`echo $Realz|awk '{ print substr(  $0, 2, 1) }'`
        set ensEnd=`echo $Realz|awk '{ print substr(  $0, 5, 1) }'`
        @ nEnsMembers = $ensEnd - $ensBeg + 1
        set futRealz = "e$nEnsMembers"
        set k = $ensBeg
        set propertyValue = "r${k}i1p$physics"
        while ($k < $ensEnd)
          @ k++
          set propertyValue = "$propertyValue,r${k}i1p$physics"
        end
      else if ("$Realz" =~ *-*) then
        set kmems = `echo $Realz | sed -e 's/-/ /g' -e 's/r/ /g'`
        echo $kmems
        set propertyValue = "r${kmems[1]}i1p$physics"
        foreach k ($kmems[2-])
          set propertyValue = "$propertyValue,r${k}i1p$physics"
        end
      endif
   else
      set ensBeg=`echo $Realz|awk '{ print substr(  $0, 2, 1) }'`
      set ensEnd=$ensBeg
      set futRealz = "$ensBeg"
   endif
  echo "decodeRIP = $decodeRIP"
  echo "propertyValue = $propertyValue"
  exit 0
