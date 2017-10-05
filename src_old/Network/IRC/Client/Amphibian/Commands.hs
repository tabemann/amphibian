-- Copyright (c) 2015, Travis Bemann
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
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
-- DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
-- FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
-- SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
-- CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
-- OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
-- OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

{-# LANGUAGE OverloadedStrings #-}

module Network.IRC.Client.Amphibian.Commands
       (cmd_PASS,
        cmd_NICK,
        cmd_USER,
        cmd_OPER,
        cmd_MODE,
        cmd_QUIT,
        cmd_ERROR,
        cmd_SQUIT,
        cmd_JOIN,
        cmd_PART,
        cmd_PRIVMSG,
        cmd_NOTICE,
        cmd_NAMES,
        cmd_PING,
        cmd_PONG,
        rpl_WELCOME,
        rpl_YOURHOST,
        rpl_CREATED,
        rpl_MYINFO,
        rpl_BOUNCE,
        rpl_USERHOST,
        rpl_ISON,
        rpl_AWAY,
        rpl_UNAWAY,
        rpl_NOAWAY,
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
        rpl_TOPICWHOTIME,
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
        rpl_USERSTART,
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
        err_ALREADYREGISTERED,
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
        rpl_STATSIAUTH,
        rpl_STATSVLINE,
        rpl_STATSLLINE,
        rpl_STATSUPTIME,
        rpl_STATSOLINE,
        rpl_STATSHLINE,
        rpl_STATSSLINE,
        rpl_STATSPING,
        rpl_STATSBLINE,
        rpl_STATSGLINE,
        rpl_STATSXLINE,
        rpl_STATSDEFINE,
        rpl_STATSULINE,
        rpl_STATSDEBUG,
        rpl_STATSDLINE,
        rpl_STATSCONN,
        err_NOSERVICEHOST,
        err_OTHER,
        ctcp_ACTION,
        ctcp_FINGER,
        ctcp_VERSION,
        ctcp_SOURCE,
        ctcp_USERINFO,
        ctcp_CLIENTINFO,
        ctcp_ERRMSG,
        ctcp_PING,
        ctcp_TIME)
       
       where

import Network.IRC.Client.Amphibian.Types
import qualified Data.ByteString as B

cmd_PASS :: MessageCommand
cmd_PASS = "PASS"

cmd_NICK :: MessageCommand
cmd_NICK = "NICK"

cmd_USER :: MessageCommand
cmd_USER = "USER"

cmd_OPER :: MessageCommand
cmd_OPER = "OPER"

cmd_MODE :: MessageCommand
cmd_MODE = "MODE"

cmd_QUIT :: MessageCommand
cmd_QUIT = "QUIT"

cmd_ERROR :: MessageCommand
cmd_ERROR = "ERROR"

cmd_SQUIT :: MessageCommand
cmd_SQUIT = "SQUIT"

cmd_JOIN :: MessageCommand
cmd_JOIN = "JOIN"

cmd_PART :: MessageCommand
cmd_PART = "PART"

cmd_PRIVMSG :: MessageCommand
cmd_PRIVMSG = "PRIVMSG"

cmd_NOTICE :: MessageCommand
cmd_NOTICE = "NOTICE"

cmd_NAMES :: MessageCommand
cmd_NAMES = "NAMES"

cmd_PING :: MessageCommand
cmd_PING = "PING"

cmd_PONG :: MessageCommand
cmd_PONG = "PONG"

rpl_WELCOME :: MessageCommand
rpl_WELCOME = "001"

rpl_YOURHOST :: MessageCommand
rpl_YOURHOST = "002"

rpl_CREATED :: MessageCommand
rpl_CREATED = "003"

rpl_MYINFO :: MessageCommand
rpl_MYINFO = "004"

rpl_BOUNCE :: MessageCommand
rpl_BOUNCE = "005"

rpl_USERHOST :: MessageCommand
rpl_USERHOST = "302"

rpl_ISON :: MessageCommand
rpl_ISON = "303"

rpl_AWAY :: MessageCommand
rpl_AWAY = "301"

rpl_UNAWAY :: MessageCommand
rpl_UNAWAY = "305"

rpl_NOAWAY :: MessageCommand
rpl_NOAWAY = "306"

rpl_WHOISUSER :: MessageCommand
rpl_WHOISUSER = "311"

rpl_WHOISSERVER :: MessageCommand
rpl_WHOISSERVER = "312"

rpl_WHOISOPERATOR :: MessageCommand
rpl_WHOISOPERATOR = "313"

rpl_WHOISIDLE :: MessageCommand
rpl_WHOISIDLE = "317"

rpl_ENDOFWHOIS :: MessageCommand
rpl_ENDOFWHOIS = "318"

rpl_WHOISCHANNELS :: MessageCommand
rpl_WHOISCHANNELS = "319"

rpl_WHOWASUSER :: MessageCommand
rpl_WHOWASUSER = "314"

rpl_ENDOFWHOWAS :: MessageCommand
rpl_ENDOFWHOWAS = "369"

rpl_LISTSTART :: MessageCommand
rpl_LISTSTART = "321"

rpl_LIST :: MessageCommand
rpl_LIST = "322"

rpl_LISTEND :: MessageCommand
rpl_LISTEND = "323"

rpl_UNIQOPIS :: MessageCommand
rpl_UNIQOPIS = "325"

rpl_CHANNELMODEIS :: MessageCommand
rpl_CHANNELMODEIS = "324"

rpl_NOTOPIC :: MessageCommand
rpl_NOTOPIC = "331"

rpl_TOPIC :: MessageCommand
rpl_TOPIC = "332"

rpl_TOPICWHOTIME :: MessageCommand
rpl_TOPICWHOTIME = "333"

rpl_INVITING :: MessageCommand
rpl_INVITING = "341"

rpl_SUMMONING :: MessageCommand
rpl_SUMMONING = "342"

rpl_INVITELIST :: MessageCommand
rpl_INVITELIST = "346"

rpl_ENDOFINVITELIST :: MessageCommand
rpl_ENDOFINVITELIST = "347"

rpl_EXCEPTLIST :: MessageCommand
rpl_EXCEPTLIST = "348"

rpl_ENDOFEXCEPTLIST :: MessageCommand
rpl_ENDOFEXCEPTLIST = "349"

rpl_VERSION :: MessageCommand
rpl_VERSION = "351"

rpl_WHOREPLY :: MessageCommand
rpl_WHOREPLY = "352"

rpl_ENDOFWHO :: MessageCommand
rpl_ENDOFWHO = "315"

rpl_NAMREPLY :: MessageCommand
rpl_NAMREPLY = "353"

rpl_ENDOFNAMES :: MessageCommand
rpl_ENDOFNAMES = "366"

rpl_LINKS :: MessageCommand
rpl_LINKS = "364"

rpl_ENDOFLINKS :: MessageCommand
rpl_ENDOFLINKS = "365"

rpl_BANLIST :: MessageCommand
rpl_BANLIST = "367"

rpl_ENDOFBANLIST :: MessageCommand
rpl_ENDOFBANLIST = "368"

rpl_INFO :: MessageCommand
rpl_INFO = "371"

rpl_ENDOFINFO :: MessageCommand
rpl_ENDOFINFO = "374"

rpl_MOTDSTART :: MessageCommand
rpl_MOTDSTART = "375"

rpl_MOTD :: MessageCommand
rpl_MOTD = "372"

rpl_ENDOFMOTD :: MessageCommand
rpl_ENDOFMOTD = "376"

rpl_YOUREOPER :: MessageCommand
rpl_YOUREOPER = "381"

rpl_REHASHING :: MessageCommand
rpl_REHASHING = "382"

rpl_YOURESERVICE :: MessageCommand
rpl_YOURESERVICE = "383"

rpl_TIME :: MessageCommand
rpl_TIME = "391"

rpl_USERSSTART :: MessageCommand
rpl_USERSSTART = "392"

rpl_USERS :: MessageCommand
rpl_USERS = "393"

rpl_ENDOFUSERS :: MessageCommand
rpl_ENDOFUSERS = "394"

rpl_NOUSERS :: MessageCommand
rpl_NOUSERS = "395"

rpl_TRACELINK :: MessageCommand
rpl_TRACELINK = "200"

rpl_TRACECONNECTING :: MessageCommand
rpl_TRACECONNECTING = "201"

rpl_TRACEHANDSHAKE :: MessageCommand
rpl_TRACEHANDSHAKE = "202"

rpl_TRACEUNKNOWN :: MessageCommand
rpl_TRACEUNKNOWN = "203"

rpl_TRACEOPERATOR :: MessageCommand
rpl_TRACEOPERATOR = "204"

rpl_TRACEUSER :: MessageCommand
rpl_TRACEUSER = "205"

rpl_TRACESERVER :: MessageCommand
rpl_TRACESERVER = "206"

rpl_TRACESERVICE :: MessageCommand
rpl_TRACESERVICE = "207"

rpl_TRACENEWTYPE :: MessageCommand
rpl_TRACENEWTYPE = "208"

rpl_TRACECLASS :: MessageCommand
rpl_TRACECLASS = "209"

rpl_TRACERECONNECT :: MessageCommand
rpl_TRACERECONNECT = "210"

rpl_TRACELOG :: MessageCommand
rpl_TRACELOG = "261"

rpl_TRACEEND :: MessageCommand
rpl_TRACEEND = "262"

rpl_STATSLINKINFO :: MessageCommand
rpl_STATSLINKINFO = "211"

rpl_STATSCOMMANDS :: MessageCommand
rpl_STATSCOMMANDS = "212"

rpl_ENDOFSTATS :: MessageCommand
rpl_ENDOFSTATS = "219"

rpl_STATSUPTIME :: MessageCommand
rpl_STATSUPTIME = "242"

rpl_STATSOLINE :: MessageCommand
rpl_STATSOLINE = "243"

rpl_UMODEIS :: MessageCommand
rpl_UMODEIS = "221"

rpl_SERVLIST :: MessageCommand
rpl_SERVLIST = "234"

rpl_SERVLISTEND :: MessageCommand
rpl_SERVLISTEND = "235"

rpl_LUSERCLIENT :: MessageCommand
rpl_LUSERCLIENT = "251"

rpl_LUSEROP :: MessageCommand
rpl_LUSEROP = "252"

rpl_LUSERUNKNOWN :: MessageCommand
rpl_LUSERUNKNOWN = "253"

rpl_LUSERCHANNELS :: MessageCommand
rpl_LUSERCHANNELS = "254"

rpl_LUSERME :: MessageCommand
rpl_LUSERME = "255"

rpl_ADMINME :: MessageCommand
rpl_ADMINME = "256"

rpl_ADMINLOC1 :: MessageCommand
rpl_ADMINLOC1 = "257"

rpl_ADMINLOC2 :: MessageCommand
rpl_ADMINLOC2 = "258"

rpl_ADMINEMAIL :: MessageCommand
rpl_ADMINEMAIL = "259"

rpl_TRYAGAIN :: MessageCOmmand
rpl_TRYAGAIN = "263"

err_NOSUCHNICK :: MessageCommand
err_NOSUCHNICK = "401"

err_NOSUCHSERVER :: MessageCommand
err_NOSUCHSERVER = "402"

err_NOSUCHCHANNEL :: MessageCommand
err_NOSUCHCHANNEL = "403"

err_CANNOTSENDTOCHAN :: MessageCommand
err_CANNOTSENDTOCHAN = "404"

err_TOOMANYCHANNELS :: MessageCommand
err_TOOMANYCHANNELS = "405"

err_WASNOSUCHNICK :: MessageCommand
err_WASNOSUCHNICK = "406"

err_TOOMANYTARGETS :: MessageCommand
err_TOOMANYTARGETS = "407"

err_NOSUCHSERVICE :: MessageCommand
err_NOSUCHSERVICE = "408"

err_NOORIGIN :: MessageCommand
err_NOORIGIN = "409"

err_NORECIPIENT :: MessageCommand
err_NORECIPIENT = "411"

err_NOTEXTTOSEND :: MessageCommand
err_NOTEXTTOSEND = "412"

err_NOTOPLEVEL :: MessageCommand
err_NOTOPLEVEL = "413"

err_WILDTOPLEVEL :: MessageCommand
err_WILDTOPLEVEL = "414"

err_BADMASK :: MessageCommand
err_BADMASK = "415"

err_UNKNOWNCOMMAND :: MessageCommand
err_UNKNOWNCOMMAND = "421"

err_NOMOTD :: MessageCommand
err_NOMOTD = "422"

err_NOADMININFO :: MessageCommand
err_NOADMININFO = "423"

err_FILEERROR :: MessageCommand
err_FILEERROR = "424"

err_NONICKNAMEGIVEN :: MessageCommand
err_NONICKNAMEGIVEN = "431"

err_ERRONEUSNICKNAME :: MessageCommand
err_ERRONEUSNICKNAME = "432"

err_NICKNAMEINUSE :: MessageCommand
err_NICKNAMEINUSE = "433"

err_NICKCOLLISION :: MessageCommand
err_NICKCOLLISION = "436"

err_UNAVAILRESOURCE :: MessageCommand
err_UNAVAILRESOURCE = "437"

err_USERNOTINCHANNEL :: MessageCommand
err_USERNOTINCHANNEL = "441"

err_NOTONCHANNEL :: MessageCommand
err_NOTONCHANNEL = "442"

err_USERONCHANNEL :: MessageCommand
err_USERONCHANNEL = "443"

err_NOLOGIN :: MessageCommand
err_NOLOGIN = "444"

err_SUMMONDISABLED :: MessageCommand
err_SUMMONDISABLED = "445"

err_USERSDISABLED :: MessageCommand
err_USERSDISABLED = "446"

err_NOTREGISTERED :: MessageCommand
err_NOTREGISTERED = "451"

err_NEEDMOREPARAMS :: MessageCommand
err_NEEDMOREPARAMS = "461"

err_ALREADYREGISTERED :: MessageCommand
err_ALREADYREGISTERED = "462"

err_NOPERMFORHOST :: MessageCommand
err_NOPERMFORHOST = "463"

err_PASSWDMISMATCH :: MessageCommand
err_PASSWDMISMATCH = "464"

err_YOUREBANNEDCREEP :: MessageCommand
err_YOUREBANNEDCREEP = "465"

err_YOUWILLBEBANNED :: MessageCommand
err_YOUWILLBEBANNED = "466"

err_KEYSET :: MessageCommand
err_KEYSET = "467"

err_CHANNELISFULL :: MessageCommand
err_CHANNELISFULL = "471"

err_UNKNOWNMODE :: MessageCommand
err_UNKNOWNMODE = "472"

err_INVITEONLYCHAN :: MessageCommand
err_INVITEONLYCHAN = "473"

err_BANNEDFROMCHAN :: MessageCommand
err_BANNEDFROMCHAN = "474"

err_BADCHANNELKEY :: MessageCommand
err_BADCHANNELKEY = "475"

err_BADCHANMASK :: MessageCommand
err_BADCHANMASK = "476"

err_NOCHANMODES :: MessageCommand
err_NOCHANMODES = "477"

err_BANLISTFULL :: MessageCommand
err_BANLISTFULL = "478"

err_NOPRIVILEGES :: MessageCommand
err_NOPRIVILEGES = "481"

err_CHANOPRIVSNEEDED :: MessageCommand
err_CHANOPRIVSNEEDED = "482"

err_CANTKILLSERVER :: MessageCommand
err_CANTKILLSERVER = "483"

err_RESTRICTED :: MessageCommand
err_RESTRICTED = "484"

err_UNIQOPPRIVSNEEDED :: MessageCommand
err_UNIQOPPRIVSNEEDED = "485"

err_NOOPERHOST :: MessageCommand
err_NOOPERHOST = "491"

err_UMODEUNKNOWNFLAG :: MessageCommand
err_UMODEUNKNOWNFLAG = "501"

err_USERSDONTMATCH :: MessageCommand
err_USERSDONTMATCH = "502"

-- Nonstandard reply and error codes

rpl_SERVICEINFO :: MessageCommand
rpl_SERVICEINFO = "231"

rpl_ENDOFSERVICES :: MessageCommand
rpl_ENDOFSERVICES = "232"

rpl_SERVICE :: MessageCommand
rpl_SERVICE = "233"

rpl_NONE :: MessageCommand
rpl_NONE = "300"

rpl_WHOISCHANOP :: MessageCommand
rpl_WHOISCHANOP = "316"

rpl_KILLDONE :: MessageCommand
rpl_KILLDONE = "361"

rpl_CLOSING :: MessageCommand
rpl_CLOSING = "362"

rpl_CLOSEEND :: MessageCommand
rpl_CLOSEEND = "363"

rpl_INFOSTART :: MessageCommand
rpl_INFOSTART = "373"

rpl_MYPORTIS :: MessageCommand
rpl_MYPORTIS = "384"

rpl_STATSCLINE :: MessageCommand
rpl_STATSCLINE = "213"

rpl_STATSNLINE :: MessageCommand
rpl_STATSNLINE = "214"

rpl_STATSILINE :: MessageCommand
rpl_STATSILINE = "215"

rpl_STATSKLINE :: MessageCommand
rpl_STATSKLINE = "216"

rpl_STATSQLINE :: MessageCommand
rpl_STATSQLINE = "217"

rpl_STATSYLINE :: MessageCommand
rpl_STATSYLINE = "218"

rpl_STATSIAUTH :: MessageCommand
rpl_STATSIAUTH = "239"

rpl_STATSVLINE :: MessageCommand
rpl_STATSVLINE = "240"

rpl_STATSLLINE :: MessageCommand
rpl_STATSLLINE = "241"

rpl_STATSUPTIME :: MessageCommand
rpl_STATSUPTIME = "242"

rpl_STATSOLINE :: MessageCommand
rpl_STATSOLINE = "243"

rpl_STATSHLINE :: MessageCommand
rpl_STATSHLINE = "244"

rpl_STATSSLINE :: MessageCommand
rpl_STATSSLINE = "245"

rpl_STATSPING :: MessageCommand
rpl_STATSPING = "246"

rpl_STATSBLINE :: MessageCommand
rpl_STATSBLINE = "247"

rpl_STATSGLINE :: MessageCommand
rpl_STATSGLINE = "247"

rpl_STATSXLINE :: MessageCommand
rpl_STATSXLINE = "247"

rpl_STATSDEFINE :: MessageCommand
rpl_STATSDEFINE = "248"

rpl_STATSULINE :: MessageCommand
rpl_STATSULINE = "248"

rpl_STATSDEBUG :: MessageCommand
rpl_STATSDEBUG = "249"

rpl_STATSDLINE :: MessageCommand
rpl_STATSDLINE = "250"

rpl_STATSCONN :: MessageCommand
rpl_STATSCONN = "250"

err_NOSERVICEHOST :: MessageCommand
err_NOSERVICEHOST = "492"

err_OTHER :: MessageCommand
err_OTHER = "500"

-- CTCP requests/replies

ctcp_ACTION :: CtcpCommand
ctcp_ACTION = "ACTION"

ctcp_FINGER :: CtcpCommand
ctcp_FINGER = "FINGER"

ctcp_VERSION :: CtcpCommand
ctcp_VERSION = "VERSION"

ctcp_SOURCE :: CtcpCommand
ctcp_SOURCE = "SOURCE"

ctcp_USERINFO :: CtcpCommand
ctcp_USERINFO = "USERINFO"

ctcp_CLIENTINFO :: CtcpCommand
ctcp_CLIENTINFO = "CLIENTINFO"

ctcp_ERRMSG :: CtcpCommand
ctcp_ERRMSG = "ERRMSG"

ctcp_PING :: CtcpCommand
ctcp_PING = "PING"

ctcp_TIME :: CtcpCommand
ctcp_TIME = "TIME"
