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
        err_OTHER)
       
       where

import Network.IRC.Client.Amphibian.Types
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as BUTF8

cmd_PASS :: MessageCommand
cmd_PASS = BUTF8.fromString "PASS"

cmd_NICK :: MessageCommand
cmd_NICK = BUTF8.fromString "NICK"

cmd_USER :: MessageCommand
cmd_USER = BUTF8.fromString "USER"

cmd_OPER :: MessageCommand
cmd_OPER = BUTF8.fromString "OPER"

cmd_MODE :: MessageCommand
cmd_MODE = BUTF8.fromString "MODE"

cmd_QUIT :: MessageCommand
cmd_QUIT = BUTF8.fromString "QUIT"

cmd_ERROR :: MessageCommand
cmd_ERROR = BUTF8.fromString "ERROR"

cmd_SQUIT :: MessageCommand
cmd_SQUIT = BUTF8.fromString "SQUIT"

cmd_JOIN :: MessageCommand
cmd_JOIN = BUTF8.fromString "JOIN"

cmd_PART :: MessageCommand
cmd_PART = BUTF8.fromString "PART"

cmd_PRIVMSG :: MessageCommand
cmd_PRIVMSG = BUTF8.fromString "PRIVMSG"

cmd_NOTICE :: MessageCommand
cmd_NOTICE = BUTF8.fromString "NOTICE"

cmd_NAMES :: MessageCommand
cmd_NAMES = BUTF8.fromString "NAMES"

rpl_WELCOME :: MessageCommand
rpl_WELCOME = BUTF8.fromString "001"

rpl_YOURHOST :: MessageCommand
rpl_YOURHOST = BUTF8.fromString "002"

rpl_CREATED :: MessageCommand
rpl_CREATED = BUTF8.fromString "003"

rpl_MYINFO :: MessageCommand
rpl_MYINFO = BUTF8.fromString "004"

rpl_BOUNCE :: MessageCommand
rpl_BOUNCE = BUTF8.fromString "005"

rpl_USERHOST :: MessageCommand
rpl_USERHOST = BUTF8.fromString "302"

rpl_ISON :: MessageCommand
rpl_ISON = BUTF8.fromString "303"

rpl_AWAY :: MessageCommand
rpl_AWAY = BUTF8.fromString "301"

rpl_UNAWAY :: MessageCommand
rpl_UNAWAY = BUTF8.fromString "305"

rpl_NOAWAY :: MessageCommand
rpl_NOAWAY = BUTF8.fromString "306"

rpl_WHOISUSER :: MessageCommand
rpl_WHOISUSER = BUTF8.fromString "311"

rpl_WHOISSERVER :: MessageCommand
rpl_WHOISSERVER = BUTF8.fromString "312"

rpl_WHOISOPERATOR :: MessageCommand
rpl_WHOISOPERATOR = BUTF8.fromString "313"

rpl_WHOISIDLE :: MessageCommand
rpl_WHOISIDLE = BUTF8.fromString "317"

rpl_ENDOFWHOIS :: MessageCommand
rpl_ENDOFWHOIS = BUTF8.fromString "318"

rpl_WHOISCHANNELS :: MessageCommand
rpl_WHOISCHANNELS = BUTF8.fromString "319"

rpl_WHOWASUSER :: MessageCommand
rpl_WHOWASUSER = BUTF8.fromString "314"

rpl_ENDOFWHOWAS :: MessageCommand
rpl_ENDOFWHOWAS = BUTF8.fromString "369"

rpl_LISTSTART :: MessageCommand
rpl_LISTSTART = BUTF8.fromString "321"

rpl_LIST :: MessageCommand
rpl_LIST = BUTF8.fromString "322"

rpl_LISTEND :: MessageCommand
rpl_LISTEND = BUTF8.fromString "323"

rpl_UNIQOPIS :: MessageCommand
rpl_UNIQOPIS = BUTF8.fromString "325"

rpl_CHANNELMODEIS :: MessageCommand
rpl_CHANNELMODEIS = BUTF8.fromString "324"

rpl_NOTOPIC :: MessageCommand
rpl_NOTOPIC = BUTF8.fromString "331"

rpl_TOPIC :: MessageCommand
rpl_TOPIC = BUTF8.fromString "332"

rpl_TOPICWHOTIME :: MessageCommand
rpl_TOPICWHOTIME = BUTF8.fromString "333"

rpl_INVITING :: MessageCommand
rpl_INVITING = BUTF8.fromString "341"

rpl_SUMMONING :: MessageCommand
rpl_SUMMONING = BUTF8.fromString "342"

rpl_INVITELIST :: MessageCommand
rpl_INVITELIST = BUTF8.fromString "346"

rpl_ENDOFINVITELIST :: MessageCommand
rpl_ENDOFINVITELIST = BUTF8.fromString "347"

rpl_EXCEPTLIST :: MessageCommand
rpl_EXCEPTLIST = BUTF8.fromString "348"

rpl_ENDOFEXCEPTLIST :: MessageCommand
rpl_ENDOFEXCEPTLIST = BUTF8.fromString "349"

rpl_VERSION :: MessageCommand
rpl_VERSION = BUTF8.fromString "351"

rpl_WHOREPLY :: MessageCommand
rpl_WHOREPLY = BUTF8.fromString "352"

rpl_ENDOFWHO :: MessageCommand
rpl_ENDOFWHO = BUTF8.fromString "315"

rpl_NAMREPLY :: MessageCommand
rpl_NAMREPLY = BUTF8.fromString "353"

rpl_ENDOFNAMES :: MessageCommand
rpl_ENDOFNAMES = BUTF8.fromString "366"

rpl_LINKS :: MessageCommand
rpl_LINKS = BUTF8.fromString "364"

rpl_ENDOFLINKS :: MessageCommand
rpl_ENDOFLINKS = BUTF8.fromString "365"

rpl_BANLIST :: MessageCommand
rpl_BANLIST = BUTF8.fromString "367"

rpl_ENDOFBANLIST :: MessageCommand
rpl_ENDOFBANLIST = BUTF8.fromString "368"

rpl_INFO :: MessageCommand
rpl_INFO = BUTF8.fromString "371"

rpl_ENDOFINFO :: MessageCommand
rpl_ENDOFINFO = BUTF8.fromString "374"

rpl_MOTDSTART :: MessageCommand
rpl_MOTDSTART = BUTF8.fromString "375"

rpl_MOTD :: MessageCommand
rpl_MOTD = BUTF8.fromString "372"

rpl_ENDOFMOTD :: MessageCommand
rpl_ENDOFMOTD = BUTF8.fromString "376"

rpl_YOUREOPER :: MessageCommand
rpl_YOUREOPER = BUTF8.fromString "381"

rpl_REHASHING :: MessageCommand
rpl_REHASHING = BUTF8.fromString "382"

rpl_YOURESERVICE :: MessageCommand
rpl_YOURESERVICE = BUTF8.fromString "383"

rpl_TIME :: MessageCommand
rpl_TIME = BUTF8.fromString "391"

rpl_USERSSTART :: MessageCommand
rpl_USERSSTART = BUTF8.fromString "392"

rpl_USERS :: MessageCommand
rpl_USERS = BUTF8.fromString "393"

rpl_ENDOFUSERS :: MessageCommand
rpl_ENDOFUSERS = BUTF8.fromString "394"

rpl_NOUSERS :: MessageCommand
rpl_NOUSERS = BUTF8.fromString "395"

rpl_TRACELINK :: MessageCommand
rpl_TRACELINK = BUTF8.fromString "200"

rpl_TRACECONNECTING :: MessageCommand
rpl_TRACECONNECTING = BUTF8.fromString "201"

rpl_TRACEHANDSHAKE :: MessageCommand
rpl_TRACEHANDSHAKE = BUTF8.fromString "202"

rpl_TRACEUNKNOWN :: MessageCommand
rpl_TRACEUNKNOWN = BUTF8.fromString "203"

rpl_TRACEOPERATOR :: MessageCommand
rpl_TRACEOPERATOR = BUTF8.fromString "204"

rpl_TRACEUSER :: MessageCommand
rpl_TRACEUSER = BUTF8.fromString "205"

rpl_TRACESERVER :: MessageCommand
rpl_TRACESERVER = BUTF8.fromString "206"

rpl_TRACESERVICE :: MessageCommand
rpl_TRACESERVICE = BUTF8.fromString "207"

rpl_TRACENEWTYPE :: MessageCommand
rpl_TRACENEWTYPE = BUTF8.fromString "208"

rpl_TRACECLASS :: MessageCommand
rpl_TRACECLASS = BUTF8.fromString "209"

rpl_TRACERECONNECT :: MessageCommand
rpl_TRACERECONNECT = BUTF8.fromString "210"

rpl_TRACELOG :: MessageCommand
rpl_TRACELOG = BUTF8.fromString "261"

rpl_TRACEEND :: MessageCommand
rpl_TRACEEND = BUTF8.fromString "262"

rpl_STATSLINKINFO :: MessageCommand
rpl_STATSLINKINFO = BUTF8.fromString "211"

rpl_STATSCOMMANDS :: MessageCommand
rpl_STATSCOMMANDS = BUTF8.fromString "212"

rpl_ENDOFSTATS :: MessageCommand
rpl_ENDOFSTATS = BUTF8.fromString "219"

rpl_STATSUPTIME :: MessageCommand
rpl_STATSUPTIME = BUTF8.fromString "242"

rpl_STATSOLINE :: MessageCommand
rpl_STATSOLINE = BUTF8.fromString "243"

rpl_UMODEIS :: MessageCommand
rpl_UMODEIS = BUTF8.fromString "221"

rpl_SERVLIST :: MessageCommand
rpl_SERVLIST = BUTF8.fromString "234"

rpl_SERVLISTEND :: MessageCommand
rpl_SERVLISTEND = BUTF8.fromString "235"

rpl_LUSERCLIENT :: MessageCommand
rpl_LUSERCLIENT = BUTF8.fromString "251"

rpl_LUSEROP :: MessageCommand
rpl_LUSEROP = BUTF8.fromString "252"

rpl_LUSERUNKNOWN :: MessageCommand
rpl_LUSERUNKNOWN = BUTF8.fromString "253"

rpl_LUSERCHANNELS :: MessageCommand
rpl_LUSERCHANNELS = BUTF8.fromString "254"

rpl_LUSERME :: MessageCommand
rpl_LUSERME = BUTF8.fromString "255"

rpl_ADMINME :: MessageCommand
rpl_ADMINME = BUTF8.fromString "256"

rpl_ADMINLOC1 :: MessageCommand
rpl_ADMINLOC1 = BUTF8.fromString "257"

rpl_ADMINLOC2 :: MessageCommand
rpl_ADMINLOC2 = BUTF8.fromString "258"

rpl_ADMINEMAIL :: MessageCommand
rpl_ADMINEMAIL = BUTF8.fromString "259"

rpl_TRYAGAIN :: MessageCOmmand
rpl_TRYAGAIN = BUTF8.fromString "263"

err_NOSUCHNICK :: MessageCommand
err_NOSUCHNICK = BUTF8.fromString "401"

err_NOSUCHSERVER :: MessageCommand
err_NOSUCHSERVER = BUTF8.fromString "402"

err_NOSUCHCHANNEL :: MessageCommand
err_NOSUCHCHANNEL = BUTF8.fromString "403"

err_CANNOTSENDTOCHAN :: MessageCommand
err_CANNOTSENDTOCHAN = BUTF8.fromString "404"

err_TOOMANYCHANNELS :: MessageCommand
err_TOOMANYCHANNELS = BUTF8.fromString "405"

err_WASNOSUCHNICK :: MessageCommand
err_WASNOSUCHNICK = BUTF8.fromString "406"

err_TOOMANYTARGETS :: MessageCommand
err_TOOMANYTARGETS = BUTF8.fromString "407"

err_NOSUCHSERVICE :: MessageCommand
err_NOSUCHSERVICE = BUTF8.fromString "408"

err_NOORIGIN :: MessageCommand
err_NOORIGIN = BUTF8.fromString "409"

err_NORECIPIENT :: MessageCommand
err_NORECIPIENT = BUTF8.fromString "411"

err_NOTEXTTOSEND :: MessageCommand
err_NOTEXTTOSEND = BUTF8.fromString "412"

err_NOTOPLEVEL :: MessageCommand
err_NOTOPLEVEL = BUTF8.fromString "413"

err_WILDTOPLEVEL :: MessageCommand
err_WILDTOPLEVEL = BUTF8.fromString "414"

err_BADMASK :: MessageCommand
err_BADMASK = BUTF8.fromString "415"

err_UNKNOWNCOMMAND :: MessageCommand
err_UNKNOWNCOMMAND = BUTF8.fromString "421"

err_NOMOTD :: MessageCommand
err_NOMOTD = BUTF8.fromString "422"

err_NOADMININFO :: MessageCommand
err_NOADMININFO = BUTF8.fromString "423"

err_FILEERROR :: MessageCommand
err_FILEERROR = BUTF8.fromString "424"

err_NONICKNAMEGIVEN :: MessageCommand
err_NONICKNAMEGIVEN = BUTF8.fromString "431"

err_ERRONEUSNICKNAME :: MessageCommand
err_ERRONEUSNICKNAME = BUTF8.fromString "432"

err_NICKNAMEINUSE :: MessageCommand
err_NICKNAMEINUSE = BUTF8.fromString "433"

err_NICKCOLLISION :: MessageCommand
err_NICKCOLLISION = BUTF8.fromString "436"

err_UNAVAILRESOURCE :: MessageCommand
err_UNAVAILRESOURCE = BUTF8.fromString "437"

err_USERNOTINCHANNEL :: MessageCommand
err_USERNOTINCHANNEL = BUTF8.fromString "441"

err_NOTONCHANNEL :: MessageCommand
err_NOTONCHANNEL = BUTF8.fromString "442"

err_USERONCHANNEL :: MessageCommand
err_USERONCHANNEL = BUTF8.fromString "443"

err_NOLOGIN :: MessageCommand
err_NOLOGIN = BUTF8.fromString "444"

err_SUMMONDISABLED :: MessageCommand
err_SUMMONDISABLED = BUTF8.fromString "445"

err_USERSDISABLED :: MessageCommand
err_USERSDISABLED = BUTF8.fromString "446"

err_NOTREGISTERED :: MessageCommand
err_NOTREGISTERED = BUTF8.fromString "451"

err_NEEDMOREPARAMS :: MessageCommand
err_NEEDMOREPARAMS = BUTF8.fromString "461"

err_ALREADYREGISTERED :: MessageCommand
err_ALREADYREGISTERED = BUTF8.fromString "462"

err_NOPERMFORHOST :: MessageCommand
err_NOPERMFORHOST = BUTF8.fromString "463"

err_PASSWDMISMATCH :: MessageCommand
err_PASSWDMISMATCH = BUTF8.fromString "464"

err_YOUREBANNEDCREEP :: MessageCommand
err_YOUREBANNEDCREEP = BUTF8.fromString "465"

err_YOUWILLBEBANNED :: MessageCommand
err_YOUWILLBEBANNED = BUTF8.fromString "466"

err_KEYSET :: MessageCommand
err_KEYSET = BUTF8.fromString "467"

err_CHANNELISFULL :: MessageCommand
err_CHANNELISFULL = BUTF8.fromString "471"

err_UNKNOWNMODE :: MessageCommand
err_UNKNOWNMODE = BUTF8.fromString "472"

err_INVITEONLYCHAN :: MessageCommand
err_INVITEONLYCHAN = BUTF8.fromString "473"

err_BANNEDFROMCHAN :: MessageCommand
err_BANNEDFROMCHAN = BUTF8.fromString "474"

err_BADCHANNELKEY :: MessageCommand
err_BADCHANNELKEY = BUTF8.fromString "475"

err_BADCHANMASK :: MessageCommand
err_BADCHANMASK = BUTF8.fromString "476"

err_NOCHANMODES :: MessageCommand
err_NOCHANMODES = BUTF8.fromString "477"

err_BANLISTFULL :: MessageCommand
err_BANLISTFULL = BUTF8.fromString "478"

err_NOPRIVILEGES :: MessageCommand
err_NOPRIVILEGES = BUTF8.fromString "481"

err_CHANOPRIVSNEEDED :: MessageCommand
err_CHANOPRIVSNEEDED = BUTF8.fromString "482"

err_CANTKILLSERVER :: MessageCommand
err_CANTKILLSERVER = BUTF8.fromString "483"

err_RESTRICTED :: MessageCommand
err_RESTRICTED = BUTF8.fromString "484"

err_UNIQOPPRIVSNEEDED :: MessageCommand
err_UNIQOPPRIVSNEEDED = BUTF8.fromString "485"

err_NOOPERHOST :: MessageCommand
err_NOOPERHOST = BUTF8.fromString "491"

err_UMODEUNKNOWNFLAG :: MessageCommand
err_UMODEUNKNOWNFLAG = BUTF8.fromString "501"

err_USERSDONTMATCH :: MessageCommand
err_USERSDONTMATCH = BUTF8.fromString "502"

-- Nonstandard reply and error codes

rpl_SERVICEINFO :: MessageCommand
rpl_SERVICEINFO = BUTF8.fromString "231"

rpl_ENDOFSERVICES :: MessageCommand
rpl_ENDOFSERVICES = BUTF8.fromString "232"

rpl_SERVICE :: MessageCommand
rpl_SERVICE = BUTF8.fromString "233"

rpl_NONE :: MessageCommand
rpl_NONE = BUTF8.fromString "300"

rpl_WHOISCHANOP :: MessageCommand
rpl_WHOISCHANOP = BUTF8.fromString "316"

rpl_KILLDONE :: MessageCommand
rpl_KILLDONE = BUTF8.fromString "361"

rpl_CLOSING :: MessageCommand
rpl_CLOSING = BUTF8.fromString "362"

rpl_CLOSEEND :: MessageCommand
rpl_CLOSEEND = BUTF8.fromString "363"

rpl_INFOSTART :: MessageCommand
rpl_INFOSTART = BUTF8.fromString "373"

rpl_MYPORTIS :: MessageCommand
rpl_MYPORTIS = BUTF8.fromString "384"

rpl_STATSCLINE :: MessageCommand
rpl_STATSCLINE = BUTF8.fromString "213"

rpl_STATSNLINE :: MessageCommand
rpl_STATSNLINE = BUTF8.fromString "214"

rpl_STATSILINE :: MessageCommand
rpl_STATSILINE = BUTF8.fromString "215"

rpl_STATSKLINE :: MessageCommand
rpl_STATSKLINE = BUTF8.fromString "216"

rpl_STATSQLINE :: MessageCommand
rpl_STATSQLINE = BUTF8.fromString "217"

rpl_STATSYLINE :: MessageCommand
rpl_STATSYLINE = BUTF8.fromString "218"

rpl_STATSIAUTH :: MessageCommand
rpl_STATSIAUTH = BUTF8.fromString "239"

rpl_STATSVLINE :: MessageCommand
rpl_STATSVLINE = BUTF8.fromString "240"

rpl_STATSLLINE :: MessageCommand
rpl_STATSLLINE = BUTF8.fromString "241"

rpl_STATSUPTIME :: MessageCommand
rpl_STATSUPTIME = BUTF8.fromString "242"

rpl_STATSOLINE :: MessageCommand
rpl_STATSOLINE = BUTF8.fromString "243"

rpl_STATSHLINE :: MessageCommand
rpl_STATSHLINE = BUTF8.fromString "244"

rpl_STATSSLINE :: MessageCommand
rpl_STATSSLINE = BUTF8.fromString "245"

rpl_STATSPING :: MessageCommand
rpl_STATSPING = BUTF8.fromString "246"

rpl_STATSBLINE :: MessageCommand
rpl_STATSBLINE = BUTF8.fromString "247"

rpl_STATSGLINE :: MessageCommand
rpl_STATSGLINE = BUTF8.fromString "247"

rpl_STATSXLINE :: MessageCommand
rpl_STATSXLINE = BUTF8.fromString "247"

rpl_STATSDEFINE :: MessageCommand
rpl_STATSDEFINE = BUTF8.fromString "248"

rpl_STATSULINE :: MessageCommand
rpl_STATSULINE = BUTF8.fromString "248"

rpl_STATSDEBUG :: MessageCommand
rpl_STATSDEBUG = BUTF8.fromString "249"

rpl_STATSDLINE :: MessageCommand
rpl_STATSDLINE = BUTF8.fromString "250"

rpl_STATSCONN :: MessageCommand
rpl_STATSCONN = BUTF8.fromString "250"

err_NOSERVICEHOST :: MessageCommand
err_NOSERVICEHOST = BUTF8.fromString "492"

err_OTHER :: MessageCommand
err_OTHER = BUTF8.fromString "500"