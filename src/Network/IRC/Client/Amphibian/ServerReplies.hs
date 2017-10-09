-- Copyright (c) 2017, Travis Bemann
-- All rights reserved.
-- 
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:
-- 
-- o Redistributions of source code must retain the above copyright notice, this
--   list of conditions and the following disclaimer.
-- 
-- o Redistributions in binary form must reproduce the above copyright notice,
--   this list of conditions and the following disclaimer in the documentation
--   and/or other materials provided with the distribution.
-- 
-- o Neither the name of the copyright holder nor the names of its
--   contributors may be used to endorse or promote products derived from
--   this software without specific prior written permission.
-- 
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
-- AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
-- ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
-- LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
-- CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
-- SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
-- INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
-- CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
-- ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
-- POSSIBILITY OF SUCH DAMAGE.

{-# LANGUAGE OverloadedStrings, OverloadedLists #-}

module Network.IRC.Client.Amphibian.ServerReplies

  (rpl_WELCOME,
   rpl_YOURHOST,
   rpl_CREATED,
   rpl_MYINFO,
   rpl_BOUNCE,
   rpl_USERHOST,
   rpl_ISON,
   rpl_AWAY,
   rpl_UNAWAY,
   rpl_NOWAWAY,
   rpl_WHOISUSER,
   rpl_WHOISSERVER,
   rpl_WHOISOPERATOR,
   rpl_WHOISIDLE,
   rpl_ENDOFWHOIS,
   rpl_WHOISCHANNELS,
   rpl_WHOWASUSER,
   rpl_ENDOFWHOWAS,
   rpl_LISTSTART,
   rpl_LIST,
   rpl_LISTEND,
   rpl_UNIQOPIS,
   rpl_CHANNELMODEIS,
   rpl_NOTOPIC,
   rpl_TOPIC,
   rpl_INVITING,
   rpl_SUMMONING,
   rpl_INVITELIST,
   rpl_ENDOFINVITELIST,
   rpl_EXCEPTLIST,
   rpl_ENDOFEXCEPTLIST,
   rpl_VERSION,
   rpl_WHOREPLY,
   rpl_ENDOFWHO,
   rpl_NAMREPLY,
   rpl_ENDOFNAMES,
   rpl_LINKS,
   rpl_ENDOFLINKS,
   rpl_BANLIST,
   rpl_ENDOFBANLIST,
   rpl_INFO,
   rpl_ENDOFINFO,
   rpl_MOTDSTART,
   rpl_MOTD,
   rpl_ENDOFMOTD,
   rpl_YOUREOPER,
   rpl_REHASHING,
   rpl_YOURESERVICE,
   rpl_TIME,
   rpl_USERSSTART,
   rpl_USERS,
   rpl_ENDOFUSERS,
   rpl_NOUSERS,
   rpl_TRACELINK,
   rpl_TRACECONNECTING,
   rpl_TRACEHANDSHAKE,
   rpl_TRACEUNKNOWN,
   rpl_TRACEOPERATOR,
   rpl_TRACEUSER,
   rpl_TRACESERVER,
   rpl_TRACESERVICE,
   rpl_TRACENEWTYPE,
   rpl_TRACECLASS,
   rpl_TRACERECONNECT,
   rpl_TRACELOG,
   rpl_TRACEEND,
   rpl_STATSLINKINFO,
   rpl_STATSCOMMANDS,
   rpl_ENDOFSTATS,
   rpl_STATSUPTIME,
   rpl_STATSOLINE,
   rpl_UMODEIS,
   rpl_SERVLIST,
   rpl_SERVLISTEND,
   rpl_LUSERCLIENT,
   rpl_LUSEROP,
   rpl_LUSERUNKNOWN,
   rpl_LUSERCHANNELS,
   rpl_LUSERME,
   rpl_ADMINME,
   rpl_ADMINLOC1,
   rpl_ADMINLOC2,
   rpl_ADMINEMAIL,
   rpl_TRYAGAIN,
   err_NOSUCHNICK,
   err_NOSUCHSERVER,
   err_NOSUCHCHANNEL,
   err_CANNOTSENDTOCHAN,
   err_TOOMANYCHANNELS,
   err_WASNOSUCHNICK,
   err_TOOMANYTARGETS,
   err_NOSUCHSERVICE,
   err_NOORIGIN,
   err_NORECIPIENT,
   err_NOTEXTTOSEND,
   err_NOTOPLEVEL,
   err_WILDTOPLEVEL,
   err_BADMASK,
   err_UNKNOWNCOMMAND,
   err_NOMOTD,
   err_NOADMININFO,
   err_FILEERROR,
   err_NONICKNAMEGIVEN,
   err_ERRONEUSNICKNAME,
   err_NICKNAMEINUSE,
   err_NICKCOLLISION,
   err_UNAVAILRESOURCE,
   err_USERNOTINCHANNEL,
   err_NOTONCHANNEL,
   err_USERONCHANNEL,
   err_NOLOGIN,
   err_SUMMONDISABLED,
   err_USERSDISABLED,
   err_NOTREGISTERED,
   err_NEEDMOREPARAMS,
   err_ALREADYREGISTRED,
   err_NOPERMFORHOST,
   err_PASSWDMISMATCH,
   err_YOUREBANNEDCREEP,
   err_YOUWILLBEBANNED,
   err_KEYSET,
   err_CHANNELISFULL,
   err_UNKNOWNMODE,
   err_INVITEONLYCHAN,
   err_BANNEDFROMCHAN,
   err_BADCHANNELKEY,
   err_BADCHANMASK,
   err_NOCHANMODES,
   err_BANLISTFULL,
   err_NOPRIVILEGES,
   err_CHANOPRIVSNEEDED,
   err_CANTKILLSERVER,
   err_RESTRICTED,
   err_UNIQOPPRIVSNEEDED,
   err_NOOPERHOST,
   err_UMODEUNKNOWNFLAG,
   err_USERSDONTMATCH,
   rpl_SERVICEINFO,
   rpl_ENDOFSERVICES,
   rpl_SERVICE,
   rpl_NONE,
   rpl_WHOISCHANOP,
   rpl_KILLDONE,
   rpl_CLOSING,
   rpl_CLOSEEND,
   rpl_INFOSTART,
   rpl_MYPORTIS,
   rpl_STATSCLINE,
   rpl_STATSNLINE,
   rpl_STATSILINE,
   rpl_STATSKLINE,
   rpl_STATSQLINE,
   rpl_STATSYLINE,
   rpl_STATSVLINE,
   rpl_STATSLLINE,
   rpl_STATSHLINE,
   rpl_STATSSLINE,
   rpl_STATSPING,
   rpl_STATSBLINE,
   rpl_STATSDLINE,
   err_NOSERVICEHOST)

  where

import qualified Data.ByteString as B
import Data.Text.Encoding (encodeUtf8)

rpl_WELCOME :: B.ByteString
rpl_WELCOME = encodeUtf8 "001"

rpl_YOURHOST :: B.ByteString
rpl_YOURHOST = encodeUtf8 "002"

rpl_CREATED :: B.ByteString
rpl_CREATED = encodeUtf8 "003"

rpl_MYINFO :: B.ByteString
rpl_MYINFO = encodeUtf8 "004"

rpl_BOUNCE :: B.ByteString
rpl_BOUNCE = encodeUtf8 "005"

rpl_USERHOST :: B.ByteString
rpl_USERHOST = encodeUtf8 "302"

rpl_ISON :: B.ByteString
rpl_ISON = encodeUtf8 "303"

rpl_AWAY :: B.ByteString
rpl_AWAY = encodeUtf8 "301"

rpl_UNAWAY :: B.ByteString
rpl_UNAWAY = encodeUtf8 "305"

rpl_NOWAWAY :: B.ByteString
rpl_NOWAWAY = encodeUtf8 "306"

rpl_WHOISUSER :: B.ByteString
rpl_WHOISUSER = encodeUtf8 "311"

rpl_WHOISSERVER :: B.ByteString
rpl_WHOISSERVER = encodeUtf8 "312"

rpl_WHOISOPERATOR :: B.ByteString
rpl_WHOISOPERATOR = encodeUtf8 "313"

rpl_WHOISIDLE :: B.ByteString
rpl_WHOISIDLE = encodeUtf8 "317"

rpl_ENDOFWHOIS :: B.ByteString
rpl_ENDOFWHOIS = encodeUtf8 "318"

rpl_WHOISCHANNELS :: B.ByteString
rpl_WHOISCHANNELS = encodeUtf8 "319"

rpl_WHOWASUSER :: B.ByteString
rpl_WHOWASUSER = encodeUtf8 "314"

rpl_ENDOFWHOWAS :: B.ByteString
rpl_ENDOFWHOWAS = encodeUtf8 "369"

rpl_LISTSTART :: B.ByteString
rpl_LISTSTART = encodeUtf8 "321"

rpl_LIST :: B.ByteString
rpl_LIST = encodeUtf8 "322"

rpl_LISTEND :: B.ByteString
rpl_LISTEND = encodeUtf8 "323"

rpl_UNIQOPIS :: B.ByteString
rpl_UNIQOPIS = encodeUtf8 "325"

rpl_CHANNELMODEIS :: B.ByteString
rpl_CHANNELMODEIS = encodeUtf8 "324"

rpl_NOTOPIC :: B.ByteString
rpl_NOTOPIC = encodeUtf8 "331"

rpl_TOPIC :: B.ByteString
rpl_TOPIC = encodeUtf8 "332"

rpl_INVITING :: B.ByteString
rpl_INVITING = encodeUtf8 "341"

rpl_SUMMONING :: B.ByteString
rpl_SUMMONING = encodeUtf8 "342"

rpl_INVITELIST :: B.ByteString
rpl_INVITELIST = encodeUtf8 "346"

rpl_ENDOFINVITELIST :: B.ByteString
rpl_ENDOFINVITELIST = encodeUtf8 "347"

rpl_EXCEPTLIST :: B.ByteString
rpl_EXCEPTLIST = encodeUtf8 "348"

rpl_ENDOFEXCEPTLIST :: B.ByteString
rpl_ENDOFEXCEPTLIST = encodeUtf8 "349"

rpl_VERSION :: B.ByteString
rpl_VERSION = encodeUtf8 "351"

rpl_WHOREPLY :: B.ByteString
rpl_WHOREPLY = encodeUtf8 "352"

rpl_ENDOFWHO :: B.ByteString
rpl_ENDOFWHO = encodeUtf8 "315"

rpl_NAMREPLY :: B.ByteString
rpl_NAMREPLY = encodeUtf8 "353"

rpl_ENDOFNAMES :: B.ByteString
rpl_ENDOFNAMES = encodeUtf8 "366"

rpl_LINKS :: B.ByteString
rpl_LINKS = encodeUtf8 "364"

rpl_ENDOFLINKS :: B.ByteString
rpl_ENDOFLINKS = encodeUtf8 "365"

rpl_BANLIST :: B.ByteString
rpl_BANLIST = encodeUtf8 "367"

rpl_ENDOFBANLIST :: B.ByteString
rpl_ENDOFBANLIST = encodeUtf8 "368"

rpl_INFO :: B.ByteString
rpl_INFO = encodeUtf8 "371"

rpl_ENDOFINFO :: B.ByteString
rpl_ENDOFINFO = encodeUtf8 "374"

rpl_MOTDSTART :: B.ByteString
rpl_MOTDSTART = encodeUtf8 "375"

rpl_MOTD :: B.ByteString
rpl_MOTD = encodeUtf8 "372"

rpl_ENDOFMOTD :: B.ByteString
rpl_ENDOFMOTD = encodeUtf8 "376"

rpl_YOUREOPER :: B.ByteString
rpl_YOUREOPER = encodeUtf8 "381"

rpl_REHASHING :: B.ByteString
rpl_REHASHING = encodeUtf8 "382"

rpl_YOURESERVICE :: B.ByteString
rpl_YOURESERVICE = encodeUtf8 "383"

rpl_TIME :: B.ByteString
rpl_TIME = encodeUtf8 "391"

rpl_USERSSTART :: B.ByteString
rpl_USERSSTART = encodeUtf8 "392"

rpl_USERS :: B.ByteString
rpl_USERS = encodeUtf8 "393"

rpl_ENDOFUSERS :: B.ByteString
rpl_ENDOFUSERS = encodeUtf8 "394"

rpl_NOUSERS :: B.ByteString
rpl_NOUSERS = encodeUtf8 "395"

rpl_TRACELINK :: B.ByteString
rpl_TRACELINK = encodeUtf8 "200"

rpl_TRACECONNECTING :: B.ByteString
rpl_TRACECONNECTING = encodeUtf8 "201"

rpl_TRACEHANDSHAKE :: B.ByteString
rpl_TRACEHANDSHAKE = encodeUtf8 "202"

rpl_TRACEUNKNOWN :: B.ByteString
rpl_TRACEUNKNOWN = encodeUtf8 "203"

rpl_TRACEOPERATOR :: B.ByteString
rpl_TRACEOPERATOR = encodeUtf8 "204"

rpl_TRACEUSER :: B.ByteString
rpl_TRACEUSER = encodeUtf8 "205"

rpl_TRACESERVER :: B.ByteString
rpl_TRACESERVER = encodeUtf8 "206"

rpl_TRACESERVICE :: B.ByteString
rpl_TRACESERVICE = encodeUtf8 "207"

rpl_TRACENEWTYPE :: B.ByteString
rpl_TRACENEWTYPE = encodeUtf8 "208"

rpl_TRACECLASS :: B.ByteString
rpl_TRACECLASS = encodeUtf8 "209"

rpl_TRACERECONNECT :: B.ByteString
rpl_TRACERECONNECT = encodeUtf8 "210"

rpl_TRACELOG :: B.ByteString
rpl_TRACELOG = encodeUtf8 "261"

rpl_TRACEEND :: B.ByteString
rpl_TRACEEND = encodeUtf8 "262"

rpl_STATSLINKINFO :: B.ByteString
rpl_STATSLINKINFO = encodeUtf8 "211"

rpl_STATSCOMMANDS :: B.ByteString
rpl_STATSCOMMANDS = encodeUtf8 "212"

rpl_ENDOFSTATS :: B.ByteString
rpl_ENDOFSTATS = encodeUtf8 "219"

rpl_STATSUPTIME :: B.ByteString
rpl_STATSUPTIME = encodeUtf8 "242"

rpl_STATSOLINE :: B.ByteString
rpl_STATSOLINE = encodeUtf8 "243"

rpl_UMODEIS :: B.ByteString
rpl_UMODEIS = encodeUtf8 "221"

rpl_SERVLIST :: B.ByteString
rpl_SERVLIST = encodeUtf8 "234"

rpl_SERVLISTEND :: B.ByteString
rpl_SERVLISTEND = encodeUtf8 "235"

rpl_LUSERCLIENT :: B.ByteString
rpl_LUSERCLIENT = encodeUtf8 "251"

rpl_LUSEROP :: B.ByteString
rpl_LUSEROP = encodeUtf8 "252"

rpl_LUSERUNKNOWN :: B.ByteString
rpl_LUSERUNKNOWN = encodeUtf8 "253"

rpl_LUSERCHANNELS :: B.ByteString
rpl_LUSERCHANNELS = encodeUtf8 "254"

rpl_LUSERME :: B.ByteString
rpl_LUSERME = encodeUtf8 "255"

rpl_ADMINME :: B.ByteString
rpl_ADMINME = encodeUtf8 "256"

rpl_ADMINLOC1 :: B.ByteString
rpl_ADMINLOC1 = encodeUtf8 "257"

rpl_ADMINLOC2 :: B.ByteString
rpl_ADMINLOC2 = encodeUtf8 "258"

rpl_ADMINEMAIL :: B.ByteString
rpl_ADMINEMAIL = encodeUtf8 "259"

rpl_TRYAGAIN :: B.ByteString
rpl_TRYAGAIN = encodeUtf8 "263"

err_NOSUCHNICK :: B.ByteString
err_NOSUCHNICK = encodeUtf8 "401"

err_NOSUCHSERVER :: B.ByteString
err_NOSUCHSERVER = encodeUtf8 "402"

err_NOSUCHCHANNEL :: B.ByteString
err_NOSUCHCHANNEL = encodeUtf8 "403"

err_CANNOTSENDTOCHAN :: B.ByteString
err_CANNOTSENDTOCHAN = encodeUtf8 "404"

err_TOOMANYCHANNELS :: B.ByteString
err_TOOMANYCHANNELS = encodeUtf8 "405"

err_WASNOSUCHNICK :: B.ByteString
err_WASNOSUCHNICK = encodeUtf8 "406"

err_TOOMANYTARGETS :: B.ByteString
err_TOOMANYTARGETS = encodeUtf8 "407"

err_NOSUCHSERVICE :: B.ByteString
err_NOSUCHSERVICE = encodeUtf8 "408"

err_NOORIGIN :: B.ByteString
err_NOORIGIN = encodeUtf8 "409"

err_NORECIPIENT :: B.ByteString
err_NORECIPIENT = encodeUtf8 "411"

err_NOTEXTTOSEND :: B.ByteString
err_NOTEXTTOSEND = encodeUtf8 "412"

err_NOTOPLEVEL :: B.ByteString
err_NOTOPLEVEL = encodeUtf8 "413"

err_WILDTOPLEVEL :: B.ByteString
err_WILDTOPLEVEL = encodeUtf8 "414"

err_BADMASK :: B.ByteString
err_BADMASK = encodeUtf8 "415"

err_UNKNOWNCOMMAND :: B.ByteString
err_UNKNOWNCOMMAND = encodeUtf8 "421"

err_NOMOTD :: B.ByteString
err_NOMOTD = encodeUtf8 "422"

err_NOADMININFO :: B.ByteString
err_NOADMININFO = encodeUtf8 "423"

err_FILEERROR :: B.ByteString
err_FILEERROR = encodeUtf8 "424"

err_NONICKNAMEGIVEN :: B.ByteString
err_NONICKNAMEGIVEN = encodeUtf8 "431"

err_ERRONEUSNICKNAME :: B.ByteString
err_ERRONEUSNICKNAME = encodeUtf8 "432"

err_NICKNAMEINUSE :: B.ByteString
err_NICKNAMEINUSE = encodeUtf8 "433"

err_NICKCOLLISION :: B.ByteString
err_NICKCOLLISION = encodeUtf8 "436"

err_UNAVAILRESOURCE :: B.ByteString
err_UNAVAILRESOURCE = encodeUtf8 "437"

err_USERNOTINCHANNEL :: B.ByteString
err_USERNOTINCHANNEL = encodeUtf8 "441"

err_NOTONCHANNEL :: B.ByteString
err_NOTONCHANNEL = encodeUtf8 "442"

err_USERONCHANNEL :: B.ByteString
err_USERONCHANNEL = encodeUtf8 "443"

err_NOLOGIN :: B.ByteString
err_NOLOGIN = encodeUtf8 "444"

err_SUMMONDISABLED :: B.ByteString
err_SUMMONDISABLED = encodeUtf8 "445"

err_USERSDISABLED :: B.ByteString
err_USERSDISABLED = encodeUtf8 "446"

err_NOTREGISTERED :: B.ByteString
err_NOTREGISTERED = encodeUtf8 "451"

err_NEEDMOREPARAMS :: B.ByteString
err_NEEDMOREPARAMS = encodeUtf8 "461"

err_ALREADYREGISTRED :: B.ByteString
err_ALREADYREGISTRED = encodeUtf8 "462"

err_NOPERMFORHOST :: B.ByteString
err_NOPERMFORHOST = encodeUtf8 "463"

err_PASSWDMISMATCH :: B.ByteString
err_PASSWDMISMATCH = encodeUtf8 "464"

err_YOUREBANNEDCREEP :: B.ByteString
err_YOUREBANNEDCREEP = encodeUtf8 "465"

err_YOUWILLBEBANNED :: B.ByteString
err_YOUWILLBEBANNED = encodeUtf8 "466"

err_KEYSET :: B.ByteString
err_KEYSET = encodeUtf8 "467"

err_CHANNELISFULL :: B.ByteString
err_CHANNELISFULL = encodeUtf8 "471"

err_UNKNOWNMODE :: B.ByteString
err_UNKNOWNMODE = encodeUtf8 "472"

err_INVITEONLYCHAN :: B.ByteString
err_INVITEONLYCHAN = encodeUtf8 "473"

err_BANNEDFROMCHAN :: B.ByteString
err_BANNEDFROMCHAN = encodeUtf8 "474"

err_BADCHANNELKEY :: B.ByteString
err_BADCHANNELKEY = encodeUtf8 "475"

err_BADCHANMASK :: B.ByteString
err_BADCHANMASK = encodeUtf8 "476"

err_NOCHANMODES :: B.ByteString
err_NOCHANMODES = encodeUtf8 "477"

err_BANLISTFULL :: B.ByteString
err_BANLISTFULL = encodeUtf8 "478"

err_NOPRIVILEGES :: B.ByteString
err_NOPRIVILEGES = encodeUtf8 "481"

err_CHANOPRIVSNEEDED :: B.ByteString
err_CHANOPRIVSNEEDED = encodeUtf8 "482"

err_CANTKILLSERVER :: B.ByteString
err_CANTKILLSERVER = encodeUtf8 "483"

err_RESTRICTED :: B.ByteString
err_RESTRICTED = encodeUtf8 "484"

err_UNIQOPPRIVSNEEDED :: B.ByteString
err_UNIQOPPRIVSNEEDED = encodeUtf8 "485"

err_NOOPERHOST :: B.ByteString
err_NOOPERHOST = encodeUtf8 "491"

err_UMODEUNKNOWNFLAG :: B.ByteString
err_UMODEUNKNOWNFLAG = encodeUtf8 "501"

err_USERSDONTMATCH :: B.ByteString
err_USERSDONTMATCH = encodeUtf8 "502"

rpl_SERVICEINFO :: B.ByteString
rpl_SERVICEINFO = encodeUtf8 "231"

rpl_ENDOFSERVICES :: B.ByteString
rpl_ENDOFSERVICES = encodeUtf8 "232"

rpl_SERVICE :: B.ByteString
rpl_SERVICE = encodeUtf8 "233"

rpl_NONE :: B.ByteString
rpl_NONE = encodeUtf8 "300"

rpl_WHOISCHANOP :: B.ByteString
rpl_WHOISCHANOP = encodeUtf8 "316"

rpl_KILLDONE :: B.ByteString
rpl_KILLDONE = encodeUtf8 "361"

rpl_CLOSING :: B.ByteString
rpl_CLOSING = encodeUtf8 "362"

rpl_CLOSEEND :: B.ByteString
rpl_CLOSEEND = encodeUtf8 "363"

rpl_INFOSTART :: B.ByteString
rpl_INFOSTART = encodeUtf8 "373"

rpl_MYPORTIS :: B.ByteString
rpl_MYPORTIS = encodeUtf8 "384"

rpl_STATSCLINE :: B.ByteString
rpl_STATSCLINE = encodeUtf8 "213"

rpl_STATSNLINE :: B.ByteString
rpl_STATSNLINE = encodeUtf8 "214"

rpl_STATSILINE :: B.ByteString
rpl_STATSILINE = encodeUtf8 "215"

rpl_STATSKLINE :: B.ByteString
rpl_STATSKLINE = encodeUtf8 "216"

rpl_STATSQLINE :: B.ByteString
rpl_STATSQLINE = encodeUtf8 "217"

rpl_STATSYLINE :: B.ByteString
rpl_STATSYLINE = encodeUtf8 "218"

rpl_STATSVLINE :: B.ByteString
rpl_STATSVLINE = encodeUtf8 "240"

rpl_STATSLLINE :: B.ByteString
rpl_STATSLLINE = encodeUtf8 "241"

rpl_STATSHLINE :: B.ByteString
rpl_STATSHLINE = encodeUtf8 "244"

rpl_STATSSLINE :: B.ByteString
rpl_STATSSLINE = encodeUtf8 "244"

rpl_STATSPING :: B.ByteString
rpl_STATSPING = encodeUtf8 "246"

rpl_STATSBLINE :: B.ByteString
rpl_STATSBLINE = encodeUtf8 "247"

rpl_STATSDLINE :: B.ByteString
rpl_STATSDLINE = encodeUtf8 "250"

err_NOSERVICEHOST :: B.ByteString
err_NOSERVICEHOST = encodeUtf8 "492"
