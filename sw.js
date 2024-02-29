/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","ed529d8326f13686d1363de16a759bf5"],["/about/index.html","f3847a73f649610826ff128d196a0496"],["/archives/2023/01/index.html","bed27daaa0a022fdcf3abb2f6499a339"],["/archives/2023/02/index.html","bbbd5f1231555e1492cb70c48d4d81a1"],["/archives/2023/02/page/2/index.html","a22b0ea79831f8f848fba7ce151bfd02"],["/archives/2023/02/page/3/index.html","8e5f7c729a09fb0bb2ff55c5196916b5"],["/archives/2023/03/index.html","c4f48705ba407c720a5adca83fe53465"],["/archives/2023/05/index.html","bc37006d8aa3f70cc94dbb62f1fd80af"],["/archives/2023/06/index.html","a2f6ffaf26a333a3d582033636148fa0"],["/archives/2023/09/index.html","dd95676e16aa10410214d3a7dcccf2e4"],["/archives/2023/11/index.html","e2f03c755ccba88868e032fb45d6f7a7"],["/archives/2023/12/index.html","8fb14acdf9a08f90314a4e1f3c0df7bd"],["/archives/2023/index.html","c04b6ebb61fbcd738916f5ff9b13ff55"],["/archives/2023/page/2/index.html","f9c0d89a66753987d139f5c01d9f3705"],["/archives/2023/page/3/index.html","110968fbd536f4cda8438783065783d8"],["/archives/2023/page/4/index.html","bfe2406dfd19aa08cf106f4cb48a0ffb"],["/archives/2023/page/5/index.html","353453cdae2409e17cfab547f72bfcc2"],["/archives/2024/02/index.html","f0e242379e706f4977da182e1d807de0"],["/archives/2024/index.html","a241a09fcd74e09081ae4504cb7add7c"],["/archives/index.html","f1b40910f44957a30a99fe2d1c28a773"],["/archives/page/2/index.html","df8e840ac976302b80b6d98c82bfd453"],["/archives/page/3/index.html","da23ec4677057ae2350519db53fa7154"],["/archives/page/4/index.html","fc66c8d6d3edbad2764d27344dae1ea2"],["/archives/page/5/index.html","781b801cb533a7de574f78271d8d5db9"],["/baidu_verify_codeva-qQP2iZOMLX.html","0d86c41ce6f2e0516c72362e952e13f1"],["/categories/Java/index.html","be5d75dadb597dc9e7fae88ab43432bb"],["/categories/Java/后端/index.html","e9776f4305ee8e55768fc03ec2b202f9"],["/categories/Java/基础/index.html","d0ec8b666156580ead427b7437ae2ee3"],["/categories/Java/基础/集合/index.html","b7cd487d2c392b95af64554d664fb57e"],["/categories/Python/index.html","c7839bb84b2b47ae34fe2aef4876cdb7"],["/categories/Python/编程环境/index.html","4f8c317248149f4564fc8009f4ade17f"],["/categories/R语言/index.html","faaca9885bd7323da6b5fd7012ded45a"],["/categories/R语言/编程环境/index.html","672b07c9515b6aaa32edb89944ffacd4"],["/categories/iPad/index.html","2a64ff8f2fd75c9a3f92c07430b10691"],["/categories/index.html","647233696305f4bb1004ad830ac0f12c"],["/categories/中间件/index.html","da0ca587b9ba9f06f6aed749a3fae10f"],["/categories/前端/Vue/index.html","48626ed2cf6fd3a435bfca0db8f84cbe"],["/categories/前端/index.html","c6e7878608c0ea4d4f2cc15c0d23ee02"],["/categories/大数据开发/ElasticSearch/index.html","59e95c552b166169a6653e76729ff71b"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","4e19bb956511001c1c8f43441e09d1ba"],["/categories/大数据开发/HBase/index.html","64697b7fe11b86b079d6c187782ea232"],["/categories/大数据开发/HBase/学习笔记/index.html","71c9ab1bf8048c78f3c7bd679b1eb2c9"],["/categories/大数据开发/HBase/环境搭建/index.html","0dffbbab19359a52e48f4329b55b4358"],["/categories/大数据开发/Hadoop/index.html","a5b921e796254efd3d723ab3c4636468"],["/categories/大数据开发/Hadoop/技术/index.html","f020e9c623407e8028eb4a91ed5db0a8"],["/categories/大数据开发/Hadoop/环境搭建/index.html","415f03b874a7e507b6e02d56ce230ac6"],["/categories/大数据开发/Redis/index.html","e223ca589f19864ae9a799adb547a971"],["/categories/大数据开发/Redis/技术/index.html","d21a1bd419327fd63c84866f4fcf40b1"],["/categories/大数据开发/Redis/环境搭建/index.html","26ba7d338317339c8c6e2bdc5feeca09"],["/categories/大数据开发/Spark/index.html","79927267e599b89b83443fbf145e4c11"],["/categories/大数据开发/Spark/环境搭建/index.html","590f8ad7d2ebb21f8a6099ba81c8b2ee"],["/categories/大数据开发/Zookeeper/index.html","be88ef99cb7873da7334281184f4d3c2"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","87574653191099b30fab29ba59c959bc"],["/categories/大数据开发/index.html","34d1f72aaa3950659cf4a9059efc79e3"],["/categories/学校课程/index.html","a1d6a91f8e9705327223d34b187e28d2"],["/categories/学校课程/计算机操作系统/index.html","475cdffb4d1a707290119785d8e96bdf"],["/categories/操作系统/Linux/index.html","30c28b91e8de6105f78c7172b6569b59"],["/categories/操作系统/Mac/index.html","36c0681bd8e0ac0a07916fdb311ed17d"],["/categories/操作系统/Windows/index.html","8c7da3a5164e8dc4754f01a24ca87d38"],["/categories/操作系统/index.html","d006986802252be76028ebd2e459ea20"],["/categories/数学建模/index.html","e1a0f0e38dcf15926cd157ab60c48fce"],["/categories/数学建模/latex/index.html","6285a10dcf3b5ee07ef7f9a63e121caf"],["/categories/数学建模/优化类/index.html","d4c56b94cdcb766cd94b2110ddb1b345"],["/categories/数学建模/优化类/现代优化算法/index.html","288f8c022519f3443061f0f39d2979cc"],["/categories/数学建模/优化类/规划类/index.html","7ae84d39dc0e0ef86f20e524b9d69add"],["/categories/数学建模/绘图/index.html","14701856d66aea797d39c4aea0aec236"],["/categories/数据库/MySQL/index.html","b19f3fc75ea2b5ba1635ee6cba9c3faa"],["/categories/数据库/index.html","af5a5beed953da35a382bbe6ed91d6fc"],["/categories/数据结构和算法/index.html","63a37d9a3e278bc887ddce40801a8312"],["/categories/数据结构和算法/page/2/index.html","0f18de4afd71905294b94201c56db1bf"],["/categories/数据结构和算法/基本原理/bfs/index.html","465fe45176f3d9c6dd2d8f43e00f5f8c"],["/categories/数据结构和算法/基本原理/dfs/index.html","c9930fd3f401f4da56f815e4ee820d51"],["/categories/数据结构和算法/基本原理/index.html","286e5d3c9d9eb43f3395fa266bd63f37"],["/categories/数据结构和算法/基本原理/动态规划/index.html","3e1484db7cd810b83bd2db1a65733545"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","e26a2da642c372240d0a278cc7c736ac"],["/categories/数据结构和算法/基本原理/图论/index.html","ae05b05e6266abf51d90a4b1e398eeaf"],["/categories/数据结构和算法/基本原理/字符串/index.html","0a83b2829d91d5f313dd28fd0d5d1039"],["/categories/数据结构和算法/基本原理/排序/index.html","5a4720e25d68a03f36dfb3c56ee96245"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","d6be6977e741f4fc6af88050da5a37e1"],["/categories/数据结构和算法/基本原理/数论/index.html","81df97ffe174e127c068eaccda62549a"],["/categories/数据结构和算法/基本原理/树论/index.html","2c3b5605627773552032e530b2365496"],["/categories/数据结构和算法/基本原理/链表/index.html","f79d4fefbefc2a94e74b247894a33759"],["/categories/数据结构和算法/算法题/index.html","fab56bb51e5b237db1898c6a694fc481"],["/categories/数据结构和算法/算法题/二分查找/index.html","7d687134557275d21d6f39e4c68adfcd"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","81b45bfcccf1a8ea8aa9dbfae3629e8d"],["/categories/数据结构和算法/算法题/动态规划/index.html","eec9ba95cb45789f901ccbb3a44468e4"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","2de4120ca466dc683cd530f21f35cf46"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","0611baf86e731856510fe54c12ecd612"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","b39b76527a5edf81653515dfe098de0e"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","77e63758e422f1fdfda11fca2aca6680"],["/categories/数据结构和算法/算法题/数论/index.html","c89d526942d293b6ac19d5aa969371cd"],["/categories/数据结构和算法/算法题/栈和队列/index.html","978d8c6500da9ec6bd5f5d4858ebb853"],["/categories/数据结构和算法/算法题/树论/index.html","5b8717024ef5c829bc1cfb0a8c6ac75b"],["/categories/杂七杂八/index.html","fe493f5d78ec035fd96ee5ccc8c13d97"],["/categories/杂七杂八/博客搭建/index.html","34959b521463239784010e7b5322bd1e"],["/categories/编程工具下载/index.html","d89c5b52964b8a620242929f3ec8b6c3"],["/categories/编程环境/index.html","2360ff09972a8784910c13d79bebe169"],["/categories/编程环境/大数据/index.html","7f726630388eb75412b54e26efa72120"],["/categories/英语学习/index.html","0a0cecec554917c336b67ee5bebfc50a"],["/categories/英语学习/英语语法/index.html","ab85c8f38e27d127f9ac38d254799f89"],["/comments/index.html","14f6ce81d19a405e3932510524fcf5ed"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/background.css","cadbae6edc90b2f277452ddd24b8a931"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/twikoo2.css","0577747a2a9555baefc708447feabfad"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","3199be4a20c9740355d7f5dc65a16850"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","14c8a43799262559496fdd9dc618764d"],["/movies/index.html","e3107632d357b25f0b0e78273f47b731"],["/music/index.html","7ebf2e525037197963ad5eb404b90af2"],["/page/2/index.html","b83bc5a7965843891234839068a0fb0e"],["/page/3/index.html","c75794a083f31b08b80399f5e5c127ce"],["/page/4/index.html","ca4bfe759602696d687a8cf0e4b52c5e"],["/page/5/index.html","0fde2e9036709321b07635f348d798a7"],["/page/6/index.html","8a42712af26e11b1710454775785defa"],["/page/7/index.html","15f301e50157a2abb9447182ca357054"],["/posts/1021360842.html","06d86f1f7e2f3d678f9db60860bf395c"],["/posts/1120620192.html","c94129804caf6c53030f8f0cc724d964"],["/posts/1137707673.html","dfd3bf16983731cf9bdf5b444a11896b"],["/posts/1141628095.html","3fa71b252d02ca6830b8ddb7f6b59388"],["/posts/1168613674.html","e8a55a75bf7496b3ce9e0c984f781306"],["/posts/1219920510.html","454de8b6f1e6954b640f765bd150610a"],["/posts/1222166338.html","d99a924d6b0e295a540645c94291c641"],["/posts/1259097482.html","afc4cca64bd9bb0d1c8fec458e2f94a3"],["/posts/1271036369.html","3b29843e3acb7f04df1034e6d70f91ae"],["/posts/1312847445.html","ab11f5072e089f494dce6c3d838307de"],["/posts/135355774.html","5eb5aca35941c536780ad84b113ca250"],["/posts/1375344716.html","102f35fbc6993fa094f1a8b86d089ebc"],["/posts/1388991698.html","1a97c4ea410d553a2adea02761428f13"],["/posts/1410315814.html","adc8749ffe555cc24f44c40f4d0d92e9"],["/posts/1452790229.html","9c38a46f07bd475e23076556174f49f3"],["/posts/1470079884.html","0f96198f482d4e9a312970d74787adeb"],["/posts/1470079885.html","872c483fe95b71c0b712229b996d34b9"],["/posts/1470079886.html","531129fcadfb4a68116b781ce3cd8621"],["/posts/1470079887.html","08bb34a4b42ae9a4144b10b0a732fa45"],["/posts/1498536549.html","48d9193d65085f97f7ece0ccf0343769"],["/posts/1539568593.html","27fbaf4de306d7ccf6807c4e2fe471e7"],["/posts/1547067935.html","4bdad1e87921e18d1eadca21aaedb416"],["/posts/1557866301.html","d859afcd5497e5826cccbd30ab8fb362"],["/posts/1571776361.html","91b431110dceadcf240a00ad3de42880"],["/posts/1605124548.html","72101e9346a6d968ba5f12952ea4a38b"],["/posts/1633036852.html","7de8e1538833050a30b9d00ecf0f0ced"],["/posts/1667740714.html","bc11928e47880011bbdb0f007f5eae4d"],["/posts/1674202625.html","080e3d27bb622e19c9878f52a0cbdad4"],["/posts/1765123828.html","2fa5d682ee7ca78a197e9e509c7ea893"],["/posts/1767336200.html","cc7f8a9b81ea5c36ca2b8db551b30c9a"],["/posts/1776114197.html","b6b30089efea5a85c3db57301ccbf6f9"],["/posts/1817748743.html","62ba708e4d2db3c85724eea28c40a348"],["/posts/1925125395.html","634999963a17613a6dac600809904b33"],["/posts/1966191251.html","013f4dc276d4d543c514d146f8ecc010"],["/posts/1987617322.html","fa7aaa84afeb92c2977d92c44f5ca999"],["/posts/1999788039.html","2a1378b83e7ac83fb4289d379c867b16"],["/posts/2075104059.html","d9e17362102d23c8bde6b48f8853a03d"],["/posts/2087796737.html","54b1369555f79bff69e41374d3cf5587"],["/posts/2106547339.html","fde940da0f01b8f41d8e05458ffb0f1f"],["/posts/2207806286.html","577e6c79823cb2a7a8f69c890a9875e3"],["/posts/2225903441.html","8513da3267758842ea6c4adb106fcc61"],["/posts/2265610284.html","0bef0927a79c5470418122c32bfef666"],["/posts/2281352001.html","f2fe213e5fcfe79df940a19a9b9a118e"],["/posts/2364755265.html","aca590783a558532e496401d9320bcc5"],["/posts/2414116852.html","ca4bdf929ae3199652cdb5146d628aef"],["/posts/2421785022.html","c7dbdd07b8e9fd7fd781b86a1c263481"],["/posts/2482902029.html","2bc872534a9420d06411816dcde40158"],["/posts/2495386210.html","e3287d03e996dfb1436159084c5d562d"],["/posts/2516528882.html","12d7a948efea4ee5ce1dd22a8df2547e"],["/posts/2522177458.html","8aab0e57c406f284cfcbeb28996b2491"],["/posts/2526659543.html","770db579fe700348f74a1538f655eb70"],["/posts/2529807823.html","74061f09e117c371a9298b7457b129ca"],["/posts/2596601004.html","511be70d7301f57d76d1ca5eaad1759e"],["/posts/2697614349.html","f6a1f6af7485cf05202d5c1da267c214"],["/posts/2742438348.html","7bc571a82f1430f4c9480792a44786cb"],["/posts/2768249503.html","128eed0a3c5ff41a1c3a92737a85fc4b"],["/posts/2864584994.html","13c5f8dccfd26bb2f7580f30c3691abc"],["/posts/2888309600.html","934191868a87224036a2de374f1acf1c"],["/posts/2891591958.html","4835b64c2d6586dce161c60c6f94bae7"],["/posts/2909934084.html","10072f225e08a07241f3e8789e5fae0d"],["/posts/2920256992.html","aed6b0559f88734bc63f0a5380380173"],["/posts/2959474469.html","31e315371136ae26c9215156b06ba49a"],["/posts/3005926051.html","c0d278d2c5d93f231acd005b521d5d5a"],["/posts/309775400.html","0bdfbf8c69264fcc63690388a0d9e17a"],["/posts/3156194925.html","a9a9dc23ccdc60cf0b7fa58f3b24e754"],["/posts/3169224211.html","d8790475506ab5053e0a1755f32db283"],["/posts/3213899550.html","a83c87deb9f76eebb4347f1131a3c68c"],["/posts/3259212833.html","0041b22b3c27543236f1200691e9bb20"],["/posts/3265658309.html","8850ff871da61f282572b114fa415c96"],["/posts/3266130344.html","d511c18515a60c7ba5d89d845eeb15a3"],["/posts/3292663995.html","1772b79292016129d2ce77c59750c607"],["/posts/3297135020.html","3ef4cb5cc66fda92241d0c649d7d9e49"],["/posts/3306641566.html","ed3841cd992d9996a4edf9f497921cc8"],["/posts/3312011324.html","c162b1da60cb04bb20a9410c052a2470"],["/posts/336911618.html","4bc1a39145dac417ed7146ccabd379de"],["/posts/3402121571.html","ec443c7e55cf4706915a5478fa1e2c7a"],["/posts/3405577485.html","7699491a619781f13be843e52643014a"],["/posts/3498516849.html","514d199b8e3d878cc3dfe70404500382"],["/posts/350679531.html","c6488fdec9b2ef2005f10ddb7ffa011a"],["/posts/3513711414.html","3742b64037949da0bf21ade653c8ae97"],["/posts/3523095624.html","8f05c3251b6ce448b4a82847cd9a6e91"],["/posts/3546711884.html","50968a195089dc80078fc65a2f7c3830"],["/posts/362397694.html","953c65e9a61546eed9e3110b2577a8e5"],["/posts/3731385230.html","3eb86ad0213f2aafc910131e4a42bdf5"],["/posts/3772089482.html","803db9df68f28ea868f04087de66786e"],["/posts/386609427.html","29b111c950a892d00f00818875312647"],["/posts/4044235327.html","f3a5084b3b630a48535352bdba37a552"],["/posts/4115971639.html","799c7a8d920968d3da3e1b8b1f6c4fd1"],["/posts/4130790367.html","ccf995f0e09173a19bf03c5407e793f0"],["/posts/4131986683.html","0bddcea410ebe651ecc54ac5151db2a7"],["/posts/4177218757.html","38d9887fdbdca8c7c3eff20faf20abfe"],["/posts/4192183953.html","4ccf790102ee6a98922469a294910235"],["/posts/4223662913.html","050cd2421804acfb5ba4f0405bb03dda"],["/posts/4261103898.html","08eea73401bb11390c1efaff224b0e8d"],["/posts/4286605504.html","7ffe1a7f458022d51bcedf1c3aed30c7"],["/posts/449089913.html","48a8e69f1b6dd8bdb63ec1b8d9ffb623"],["/posts/469711973.html","76563d6d51021a04e0a39866ff9a9446"],["/posts/482495853.html","5ca1f564ebc43c4151e858e4ce2a3578"],["/posts/488247922.html","e05eea9ed9a1190f63db8630c86557f6"],["/posts/517302816.html","d2cfeb8965651e65fff24fe6e0737a2e"],["/posts/570165348.html","6f1fd88e8be47049de1354c8490e0fe9"],["/posts/595890772.html","cca4d21d7428e08c4912be0df35f1502"],["/posts/67485572.html","ad218d5985ac4d1453ba1500153900d5"],["/posts/694347442.html","50cf6fcde6f3ed66afb0c624cd5c688e"],["/posts/707384687.html","22b1afe46cf3971425f2dd52896060d4"],["/posts/71180092.html","964fd5eaaaf7720b56693444be21b02f"],["/posts/716459272.html","a67f5e7eff3bacba999d6a0fb18287e8"],["/posts/765481613.html","ad3a0554b30f0ab4ca970e7dde53fc77"],["/posts/778231993.html","944a9b6e5de02d3f10cfce29a1dcfe51"],["/posts/795397410.html","69c3e4dd17a5b7a47de8cb70b1754dec"],["/posts/820223701.html","0ee3acc65fa79df72a4af089e2639e55"],["/posts/830372185.html","3b1e171596bb9c23141ca474fc011b7f"],["/posts/88294277.html","cb59c5dc1746502f32797ee70f568d4e"],["/posts/939963535.html","57151d8b0d7b0fa7c980a4aee1a15796"],["/posts/983786067.html","62b7be377c4bf7518205e1d647e25ddf"],["/sw-register.js","c4aeb19d3da6d09823220b5201255624"],["/tags/C/index.html","4b85de50618dc803cf45c461f6dc95d8"],["/tags/C/page/2/index.html","f2e6772e38a308e66fde9732620cc65c"],["/tags/C/page/3/index.html","bf0501d8871cf9422aba11acd3147d6d"],["/tags/C/page/4/index.html","5151782de0ac7086b463823b402ecbb9"],["/tags/ETL/index.html","1e9b7812be7d439ac9a1824122db505c"],["/tags/ElasticSearch/index.html","445e0528f20a6de79f5104e57f8395e4"],["/tags/GUI/index.html","d15ac5f07d968eab8e1b73206779faf1"],["/tags/HBase/index.html","71c0988b092207f4849b096194f72fef"],["/tags/Hadoop/index.html","37aa241de28cbd0d9d9664b41382ba9c"],["/tags/Hadoop/page/2/index.html","575f19bc4e1c52b0190d03cdc8458e6d"],["/tags/Java/index.html","59f0ae6583f9557d0c3f2693ab3c8b8a"],["/tags/Java后端/index.html","87ed26c34b0f9dc711bf358f972596b6"],["/tags/Java后端/page/2/index.html","e086d38e75a11e00df71a1d995d58b49"],["/tags/Java基础/index.html","ae10d251c6f2c840a6673f67d4491a6b"],["/tags/Java基础/page/2/index.html","c47d3231bbfb6c48a2741b1c2dcca808"],["/tags/Kettle/index.html","fb659728132353581b5870c225b38329"],["/tags/Kibana/index.html","e3e4301e55608152ed3fff5829094706"],["/tags/Linux/index.html","501dc6d5f4574a15ceea747e242dd089"],["/tags/Linux/page/2/index.html","a2a4766b220960151e7a1eeb862af0d2"],["/tags/Linux/page/3/index.html","19e72f8f76e4fedaf21d4a12ea343b53"],["/tags/Mac/index.html","b8373a807c7a65c1e61f2ae99ba87073"],["/tags/Mac/page/2/index.html","2295bbffb9c5ad89bce2a63cfbd7aa19"],["/tags/Maven/index.html","50f1e4427bcfe74caa3f49e0ac43b8ab"],["/tags/MySQL/index.html","5dfd979b595afae3d2b432b98f878c98"],["/tags/Python/index.html","0307c9146051a6fe62ba056942ef8172"],["/tags/Redis/index.html","35b2d60e8284e200a60028fbd90038fe"],["/tags/R语言/index.html","ae851afc914e9118a1178a60159aedfa"],["/tags/Spark/index.html","0c067aca407f07a873601455b3cd1ad0"],["/tags/Ubuntu/index.html","d3530ab2d79a228f171754b7db053164"],["/tags/Vue/index.html","93b8fe09ccb3a86f0f429d5fdeec3bcd"],["/tags/Windows/index.html","37330edae55c3a939f9da7af6fcc6292"],["/tags/ZooKeeper/index.html","9c08599a077f1c5327292be55cdb5b7f"],["/tags/bfs/index.html","cab32c41f88d827aaa3a82c5f81e4f3c"],["/tags/dfs/index.html","825dcbb076790b18178f998cada370b0"],["/tags/folium/index.html","0db7afbbea591394f1b0d6e742e3e7bd"],["/tags/git/index.html","a4da16386c867d2e30ec01c408b60180"],["/tags/iPad找电子书/index.html","d80bb7068852e93d14001bf118a1a147"],["/tags/index.html","e3bdae2a6c179f7b6e85a7c0033f687c"],["/tags/latex/index.html","35a147cb0bc23459d434a4b27fb89b02"],["/tags/中间件/index.html","1fbbd4ff30d5237bd4f2d77734ae3c5e"],["/tags/二分查找/index.html","ba6f5ed0189f8493c1fb0c687c7813ec"],["/tags/优化类/index.html","d2182a92d9afc1b6c5f15d8aedc72d22"],["/tags/前端/index.html","0b78717def47c7be961fe980292d5fb9"],["/tags/前缀和与差分/index.html","4829dc4eb57ea74a33cbedbaa4808374"],["/tags/动态规划/index.html","2c792b4d7eaec3065b48037aff00070d"],["/tags/动态规划/page/2/index.html","ec0c35976c06e8af778104acadb60c1b"],["/tags/博客搭建/index.html","bff7e148badef8f865fddff40d50de81"],["/tags/图论/index.html","afa38940ac89e3221eabd1f891a8943c"],["/tags/大数据/index.html","4d4b2282cf647a4d52c425e29712439a"],["/tags/大数据/page/2/index.html","8a73b77d7723e914fba0d47012cc09d2"],["/tags/排序/index.html","033dee475911ffd099547a9132f6af0e"],["/tags/操作系统/index.html","f5087d64d27df120c45d82e56f0357f5"],["/tags/数学建模/index.html","b45c9b854e26095b19a496fc6035c837"],["/tags/数据库/index.html","6eb0f8f7a9deb44ae5c38a28b485d2fc"],["/tags/数据结构和算法/index.html","941402bd9cb14b29785b3ef7ccff9460"],["/tags/数据结构和算法/page/2/index.html","837a130a1941610d1548d82639e581ed"],["/tags/数据结构和算法/page/3/index.html","d26bead3fcd31fffadd5c340b63c1e40"],["/tags/数据结构和算法/page/4/index.html","f81e972c8d0ea14be9c5aa062d7de3ea"],["/tags/数据结构和算法/page/5/index.html","83a304fee27ac1bdc33e82e1be2314c2"],["/tags/数组和字符串/index.html","4b6c9ec581da652a6642b81791905236"],["/tags/数论/index.html","8a870127c7bbb3e2b1254a31ffaf4da5"],["/tags/枚举类/index.html","afffee8fc189a91170ff5e5200fae931"],["/tags/栈和队列/index.html","885dd2bcab338ebacbb1ba26aaa9aa7d"],["/tags/树论/index.html","b26ba55be2fbb980e582090fe89192a5"],["/tags/测试/index.html","ff8cd4feb9e00e3f1e96e2b10eaa89c0"],["/tags/环境/index.html","e13b3825dce1090d6f735a1992297588"],["/tags/环境变量/index.html","6924c5c4889ded7156f585eefa761e33"],["/tags/绘图/index.html","2b388b188457db2d5ffa0e5b5206b1a0"],["/tags/编程工具/index.html","aa43cd48db5a1305acdd56fbfcba496f"],["/tags/编程环境/index.html","2bef46933cff1002fc85afe4d460cbeb"],["/tags/网络编程/index.html","0d70b0ca7d02944baf405623b9004b88"],["/tags/英语语法/index.html","2db88a7c109014d6f61a0702f921d5bb"],["/tags/计算机操作系统/index.html","809c407e55e0ddf15044e4873ae1a1ac"],["/tags/论文/index.html","dc6696c44023404c67fbfb8eeab34751"],["/tags/资源下载/index.html","b1259770c32a7123791721a24b4ff1e4"],["/tags/链表/index.html","8462f769d1b5cae9cad015ff66908774"],["/tags/集合/index.html","58376fa508a213d1c0f29ef39db8fbe5"],["/tags/集群/index.html","6dda9449487b64a6eb6116a7034bc190"]];
var cacheName = 'sw-precache-v3--' + (self.registration ? self.registration.scope : '');
var firstRegister = 1; // 默认1是首次安装SW， 0是SW更新


var ignoreUrlParametersMatching = [/^utm_/];


var addDirectoryIndex = function (originalUrl, index) {
    var url = new URL(originalUrl);
    if (url.pathname.slice(-1) === '/') {
        url.pathname += index;
    }
    return url.toString();
};

var cleanResponse = function (originalResponse) {
    // 如果没有重定向响应，不需干啥
    if (!originalResponse.redirected) {
        return Promise.resolve(originalResponse);
    }

    // Firefox 50 及以下不知处 Response.body 流, 所以我们需要读取整个body以blob形式返回。
    var bodyPromise = 'body' in originalResponse ?
        Promise.resolve(originalResponse.body) :
        originalResponse.blob();

    return bodyPromise.then(function (body) {
        // new Response() 可同时支持 stream or Blob.
        return new Response(body, {
            headers: originalResponse.headers,
            status: originalResponse.status,
            statusText: originalResponse.statusText
        });
    });
};

var createCacheKey = function (originalUrl, paramName, paramValue,
    dontCacheBustUrlsMatching) {

    // 创建一个新的URL对象，避免影响原始URL
    var url = new URL(originalUrl);

    // 如果 dontCacheBustUrlsMatching 值没有设置，或是没有匹配到，将值拼接到url.serach后
    if (!dontCacheBustUrlsMatching ||
        !(url.pathname.match(dontCacheBustUrlsMatching))) {
        url.search += (url.search ? '&' : '') +
            encodeURIComponent(paramName) + '=' + encodeURIComponent(paramValue);
    }

    return url.toString();
};

var isPathWhitelisted = function (whitelist, absoluteUrlString) {
    // 如果 whitelist 是空数组，则认为全部都在白名单内
    if (whitelist.length === 0) {
        return true;
    }

    // 否则逐个匹配正则匹配并返回
    var path = (new URL(absoluteUrlString)).pathname;
    return whitelist.some(function (whitelistedPathRegex) {
        return path.match(whitelistedPathRegex);
    });
};

var stripIgnoredUrlParameters = function (originalUrl,
    ignoreUrlParametersMatching) {
    var url = new URL(originalUrl);
    // 移除 hash; 查看 https://github.com/GoogleChrome/sw-precache/issues/290
    url.hash = '';

    url.search = url.search.slice(1) // 是否包含 '?'
        .split('&') // 分割成数组 'key=value' 的形式
        .map(function (kv) {
            return kv.split('='); // 分割每个 'key=value' 字符串成 [key, value] 形式
        })
        .filter(function (kv) {
            return ignoreUrlParametersMatching.every(function (ignoredRegex) {
                return !ignoredRegex.test(kv[0]); // 如果 key 没有匹配到任何忽略参数正则，就 Return true
            });
        })
        .map(function (kv) {
            return kv.join('='); // 重新把 [key, value] 格式转换为 'key=value' 字符串
        })
        .join('&'); // 将所有参数 'key=value' 以 '&' 拼接

    return url.toString();
};


var addDirectoryIndex = function (originalUrl, index) {
    var url = new URL(originalUrl);
    if (url.pathname.slice(-1) === '/') {
        url.pathname += index;
    }
    return url.toString();
};

var hashParamName = '_sw-precache';
var urlsToCacheKeys = new Map(
    precacheConfig.map(function (item) {
        var relativeUrl = item[0];
        var hash = item[1];
        var absoluteUrl = new URL(relativeUrl, self.location);
        var cacheKey = createCacheKey(absoluteUrl, hashParamName, hash, false);
        return [absoluteUrl.toString(), cacheKey];
    })
);

function setOfCachedUrls(cache) {
    return cache.keys().then(function (requests) {
        // 如果原cacheName中没有缓存任何收，就默认是首次安装，否则认为是SW更新
        if (requests && requests.length > 0) {
            firstRegister = 0; // SW更新
        }
        return requests.map(function (request) {
            return request.url;
        });
    }).then(function (urls) {
        return new Set(urls);
    });
}

self.addEventListener('install', function (event) {
    event.waitUntil(
        caches.open(cacheName).then(function (cache) {
            return setOfCachedUrls(cache).then(function (cachedUrls) {
                return Promise.all(
                    Array.from(urlsToCacheKeys.values()).map(function (cacheKey) {
                        // 如果缓存中没有匹配到cacheKey，添加进去
                        if (!cachedUrls.has(cacheKey)) {
                            var request = new Request(cacheKey, { credentials: 'same-origin' });
                            return fetch(request).then(function (response) {
                                // 只要返回200才能继续，否则直接抛错
                                if (!response.ok) {
                                    throw new Error('Request for ' + cacheKey + ' returned a ' +
                                        'response with status ' + response.status);
                                }

                                return cleanResponse(response).then(function (responseToCache) {
                                    return cache.put(cacheKey, responseToCache);
                                });
                            });
                        }
                    })
                );
            });
        })
            .then(function () {
            
            // 强制 SW 状态 installing -> activate
            return self.skipWaiting();
            
        })
    );
});

self.addEventListener('activate', function (event) {
    var setOfExpectedUrls = new Set(urlsToCacheKeys.values());

    event.waitUntil(
        caches.open(cacheName).then(function (cache) {
            return cache.keys().then(function (existingRequests) {
                return Promise.all(
                    existingRequests.map(function (existingRequest) {
                        // 删除原缓存中相同键值内容
                        if (!setOfExpectedUrls.has(existingRequest.url)) {
                            return cache.delete(existingRequest);
                        }
                    })
                );
            });
        }).then(function () {
            
            return self.clients.claim();
            
        }).then(function () {
                // 如果是首次安装 SW 时, 不发送更新消息（是否是首次安装，通过指定cacheName 中是否有缓存信息判断）
                // 如果不是首次安装，则是内容有更新，需要通知页面重载更新
                if (!firstRegister) {
                    return self.clients.matchAll()
                        .then(function (clients) {
                            if (clients && clients.length) {
                                clients.forEach(function (client) {
                                    client.postMessage('sw.update');
                                })
                            }
                        })
                }
            })
    );
});



    self.addEventListener('fetch', function (event) {
        if (event.request.method === 'GET') {

            // 是否应该 event.respondWith()，需要我们逐步的判断
            // 而且也方便了后期做特殊的特殊
            var shouldRespond;


            // 首先去除已配置的忽略参数及hash
            // 查看缓存简直中是否包含该请求，包含就将shouldRespond 设为true
            var url = stripIgnoredUrlParameters(event.request.url, ignoreUrlParametersMatching);
            shouldRespond = urlsToCacheKeys.has(url);

            // 如果 shouldRespond 是 false, 我们在url后默认增加 'index.html'
            // (或者是你在配置文件中自行配置的 directoryIndex 参数值)，继续查找缓存列表
            var directoryIndex = 'index.html';
            if (!shouldRespond && directoryIndex) {
                url = addDirectoryIndex(url, directoryIndex);
                shouldRespond = urlsToCacheKeys.has(url);
            }

            // 如果 shouldRespond 仍是 false，检查是否是navigation
            // request， 如果是的话，判断是否能与 navigateFallbackWhitelist 正则列表匹配
            var navigateFallback = '';
            if (!shouldRespond &&
                navigateFallback &&
                (event.request.mode === 'navigate') &&
                isPathWhitelisted([], event.request.url)
            ) {
                url = new URL(navigateFallback, self.location).toString();
                shouldRespond = urlsToCacheKeys.has(url);
            }

            // 如果 shouldRespond 被置为 true
            // 则 event.respondWith()匹配缓存返回结果，匹配不成就直接请求.
            if (shouldRespond) {
                event.respondWith(
                    caches.open(cacheName).then(function (cache) {
                        return cache.match(urlsToCacheKeys.get(url)).then(function (response) {
                            if (response) {
                                return response;
                            }
                            throw Error('The cached response that was expected is missing.');
                        });
                    }).catch(function (e) {
                        // 如果捕获到异常错误，直接返回 fetch() 请求资源
                        console.warn('Couldn\'t serve response for "%s" from cache: %O', event.request.url, e);
                        return fetch(event.request);
                    })
                );
            }
        }
    });



// *** Start of auto-included sw-toolbox code. ***
/* 
 Copyright 2016 Google Inc. All Rights Reserved.

 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
*/!function(e){if("object"==typeof exports&&"undefined"!=typeof module)module.exports=e();else if("function"==typeof define&&define.amd)define([],e);else{var t;t="undefined"!=typeof window?window:"undefined"!=typeof global?global:"undefined"!=typeof self?self:this,t.toolbox=e()}}(function(){return function e(t,n,r){function o(c,s){if(!n[c]){if(!t[c]){var a="function"==typeof require&&require;if(!s&&a)return a(c,!0);if(i)return i(c,!0);var u=new Error("Cannot find module '"+c+"'");throw u.code="MODULE_NOT_FOUND",u}var f=n[c]={exports:{}};t[c][0].call(f.exports,function(e){var n=t[c][1][e];return o(n?n:e)},f,f.exports,e,t,n,r)}return n[c].exports}for(var i="function"==typeof require&&require,c=0;c<r.length;c++)o(r[c]);return o}({1:[function(e,t,n){"use strict";function r(e,t){t=t||{};var n=t.debug||m.debug;n&&console.log("[sw-toolbox] "+e)}function o(e){var t;return e&&e.cache&&(t=e.cache.name),t=t||m.cache.name,caches.open(t)}function i(e,t){t=t||{};var n=t.successResponses||m.successResponses;return fetch(e.clone()).then(function(r){return"GET"===e.method&&n.test(r.status)&&o(t).then(function(n){n.put(e,r).then(function(){var r=t.cache||m.cache;(r.maxEntries||r.maxAgeSeconds)&&r.name&&c(e,n,r)})}),r.clone()})}function c(e,t,n){var r=s.bind(null,e,t,n);d=d?d.then(r):r()}function s(e,t,n){var o=e.url,i=n.maxAgeSeconds,c=n.maxEntries,s=n.name,a=Date.now();return r("Updating LRU order for "+o+". Max entries is "+c+", max age is "+i),g.getDb(s).then(function(e){return g.setTimestampForUrl(e,o,a)}).then(function(e){return g.expireEntries(e,c,i,a)}).then(function(e){r("Successfully updated IDB.");var n=e.map(function(e){return t.delete(e)});return Promise.all(n).then(function(){r("Done with cache cleanup.")})}).catch(function(e){r(e)})}function a(e,t,n){return r("Renaming cache: ["+e+"] to ["+t+"]",n),caches.delete(t).then(function(){return Promise.all([caches.open(e),caches.open(t)]).then(function(t){var n=t[0],r=t[1];return n.keys().then(function(e){return Promise.all(e.map(function(e){return n.match(e).then(function(t){return r.put(e,t)})}))}).then(function(){return caches.delete(e)})})})}function u(e,t){return o(t).then(function(t){return t.add(e)})}function f(e,t){return o(t).then(function(t){return t.delete(e)})}function h(e){e instanceof Promise||p(e),m.preCacheItems=m.preCacheItems.concat(e)}function p(e){var t=Array.isArray(e);if(t&&e.forEach(function(e){"string"==typeof e||e instanceof Request||(t=!1)}),!t)throw new TypeError("The precache method expects either an array of strings and/or Requests or a Promise that resolves to an array of strings and/or Requests.");return e}function l(e,t,n){if(!e)return!1;if(t){var r=e.headers.get("date");if(r){var o=new Date(r);if(o.getTime()+1e3*t<n)return!1}}return!0}var d,m=e("./options"),g=e("./idb-cache-expiration");t.exports={debug:r,fetchAndCache:i,openCache:o,renameCache:a,cache:u,uncache:f,precache:h,validatePrecacheInput:p,isResponseFresh:l}},{"./idb-cache-expiration":2,"./options":4}],2:[function(e,t,n){"use strict";function r(e){return new Promise(function(t,n){var r=indexedDB.open(u+e,f);r.onupgradeneeded=function(){var e=r.result.createObjectStore(h,{keyPath:p});e.createIndex(l,l,{unique:!1})},r.onsuccess=function(){t(r.result)},r.onerror=function(){n(r.error)}})}function o(e){return e in d||(d[e]=r(e)),d[e]}function i(e,t,n){return new Promise(function(r,o){var i=e.transaction(h,"readwrite"),c=i.objectStore(h);c.put({url:t,timestamp:n}),i.oncomplete=function(){r(e)},i.onabort=function(){o(i.error)}})}function c(e,t,n){return t?new Promise(function(r,o){var i=1e3*t,c=[],s=e.transaction(h,"readwrite"),a=s.objectStore(h),u=a.index(l);u.openCursor().onsuccess=function(e){var t=e.target.result;if(t&&n-i>t.value[l]){var r=t.value[p];c.push(r),a.delete(r),t.continue()}},s.oncomplete=function(){r(c)},s.onabort=o}):Promise.resolve([])}function s(e,t){return t?new Promise(function(n,r){var o=[],i=e.transaction(h,"readwrite"),c=i.objectStore(h),s=c.index(l),a=s.count();s.count().onsuccess=function(){var e=a.result;e>t&&(s.openCursor().onsuccess=function(n){var r=n.target.result;if(r){var i=r.value[p];o.push(i),c.delete(i),e-o.length>t&&r.continue()}})},i.oncomplete=function(){n(o)},i.onabort=r}):Promise.resolve([])}function a(e,t,n,r){return c(e,n,r).then(function(n){return s(e,t).then(function(e){return n.concat(e)})})}var u="sw-toolbox-",f=1,h="store",p="url",l="timestamp",d={};t.exports={getDb:o,setTimestampForUrl:i,expireEntries:a}},{}],3:[function(e,t,n){"use strict";function r(e){var t=a.match(e.request);t?e.respondWith(t(e.request)):a.default&&"GET"===e.request.method&&0===e.request.url.indexOf("http")&&e.respondWith(a.default(e.request))}function o(e){s.debug("activate event fired");var t=u.cache.name+"$$$inactive$$$";e.waitUntil(s.renameCache(t,u.cache.name))}function i(e){return e.reduce(function(e,t){return e.concat(t)},[])}function c(e){var t=u.cache.name+"$$$inactive$$$";s.debug("install event fired"),s.debug("creating cache ["+t+"]"),e.waitUntil(s.openCache({cache:{name:t}}).then(function(e){return Promise.all(u.preCacheItems).then(i).then(s.validatePrecacheInput).then(function(t){return s.debug("preCache list: "+(t.join(", ")||"(none)")),e.addAll(t)})}))}e("serviceworker-cache-polyfill");var s=e("./helpers"),a=e("./router"),u=e("./options");t.exports={fetchListener:r,activateListener:o,installListener:c}},{"./helpers":1,"./options":4,"./router":6,"serviceworker-cache-polyfill":16}],4:[function(e,t,n){"use strict";var r;r=self.registration?self.registration.scope:self.scope||new URL("./",self.location).href,t.exports={cache:{name:"$$$toolbox-cache$$$"+r+"$$$",maxAgeSeconds:null,maxEntries:null},debug:!1,networkTimeoutSeconds:null,preCacheItems:[],successResponses:/^0|([123]\d\d)|(40[14567])|410$/}},{}],5:[function(e,t,n){"use strict";var r=new URL("./",self.location),o=r.pathname,i=e("path-to-regexp"),c=function(e,t,n,r){t instanceof RegExp?this.fullUrlRegExp=t:(0!==t.indexOf("/")&&(t=o+t),this.keys=[],this.regexp=i(t,this.keys)),this.method=e,this.options=r,this.handler=n};c.prototype.makeHandler=function(e){var t;if(this.regexp){var n=this.regexp.exec(e);t={},this.keys.forEach(function(e,r){t[e.name]=n[r+1]})}return function(e){return this.handler(e,t,this.options)}.bind(this)},t.exports=c},{"path-to-regexp":15}],6:[function(e,t,n){"use strict";function r(e){return e.replace(/[-\/\\^$*+?.()|[\]{}]/g,"\\$&")}var o=e("./route"),i=e("./helpers"),c=function(e,t){for(var n=e.entries(),r=n.next(),o=[];!r.done;){var i=new RegExp(r.value[0]);i.test(t)&&o.push(r.value[1]),r=n.next()}return o},s=function(){this.routes=new Map,this.routes.set(RegExp,new Map),this.default=null};["get","post","put","delete","head","any"].forEach(function(e){s.prototype[e]=function(t,n,r){return this.add(e,t,n,r)}}),s.prototype.add=function(e,t,n,c){c=c||{};var s;t instanceof RegExp?s=RegExp:(s=c.origin||self.location.origin,s=s instanceof RegExp?s.source:r(s)),e=e.toLowerCase();var a=new o(e,t,n,c);this.routes.has(s)||this.routes.set(s,new Map);var u=this.routes.get(s);u.has(e)||u.set(e,new Map);var f=u.get(e),h=a.regexp||a.fullUrlRegExp;f.has(h.source)&&i.debug('"'+t+'" resolves to same regex as existing route.'),f.set(h.source,a)},s.prototype.matchMethod=function(e,t){var n=new URL(t),r=n.origin,o=n.pathname;return this._match(e,c(this.routes,r),o)||this._match(e,[this.routes.get(RegExp)],t)},s.prototype._match=function(e,t,n){if(0===t.length)return null;for(var r=0;r<t.length;r++){var o=t[r],i=o&&o.get(e.toLowerCase());if(i){var s=c(i,n);if(s.length>0)return s[0].makeHandler(n)}}return null},s.prototype.match=function(e){return this.matchMethod(e.method,e.url)||this.matchMethod("any",e.url)},t.exports=new s},{"./helpers":1,"./route":5}],7:[function(e,t,n){"use strict";function r(e,t,n){return n=n||{},i.debug("Strategy: cache first ["+e.url+"]",n),i.openCache(n).then(function(t){return t.match(e).then(function(t){var r=n.cache||o.cache,c=Date.now();return i.isResponseFresh(t,r.maxAgeSeconds,c)?t:i.fetchAndCache(e,n)})})}var o=e("../options"),i=e("../helpers");t.exports=r},{"../helpers":1,"../options":4}],8:[function(e,t,n){"use strict";function r(e,t,n){return n=n||{},i.debug("Strategy: cache only ["+e.url+"]",n),i.openCache(n).then(function(t){return t.match(e).then(function(e){var t=n.cache||o.cache,r=Date.now();if(i.isResponseFresh(e,t.maxAgeSeconds,r))return e})})}var o=e("../options"),i=e("../helpers");t.exports=r},{"../helpers":1,"../options":4}],9:[function(e,t,n){"use strict";function r(e,t,n){return o.debug("Strategy: fastest ["+e.url+"]",n),new Promise(function(r,c){var s=!1,a=[],u=function(e){a.push(e.toString()),s?c(new Error('Both cache and network failed: "'+a.join('", "')+'"')):s=!0},f=function(e){e instanceof Response?r(e):u("No result returned")};o.fetchAndCache(e.clone(),n).then(f,u),i(e,t,n).then(f,u)})}var o=e("../helpers"),i=e("./cacheOnly");t.exports=r},{"../helpers":1,"./cacheOnly":8}],10:[function(e,t,n){t.exports={networkOnly:e("./networkOnly"),networkFirst:e("./networkFirst"),cacheOnly:e("./cacheOnly"),cacheFirst:e("./cacheFirst"),fastest:e("./fastest")}},{"./cacheFirst":7,"./cacheOnly":8,"./fastest":9,"./networkFirst":11,"./networkOnly":12}],11:[function(e,t,n){"use strict";function r(e,t,n){n=n||{};var r=n.successResponses||o.successResponses,c=n.networkTimeoutSeconds||o.networkTimeoutSeconds;return i.debug("Strategy: network first ["+e.url+"]",n),i.openCache(n).then(function(t){var s,a,u=[];if(c){var f=new Promise(function(r){s=setTimeout(function(){t.match(e).then(function(e){var t=n.cache||o.cache,c=Date.now(),s=t.maxAgeSeconds;i.isResponseFresh(e,s,c)&&r(e)})},1e3*c)});u.push(f)}var h=i.fetchAndCache(e,n).then(function(e){if(s&&clearTimeout(s),r.test(e.status))return e;throw i.debug("Response was an HTTP error: "+e.statusText,n),a=e,new Error("Bad response")}).catch(function(r){return i.debug("Network or response error, fallback to cache ["+e.url+"]",n),t.match(e).then(function(e){if(e)return e;if(a)return a;throw r})});return u.push(h),Promise.race(u)})}var o=e("../options"),i=e("../helpers");t.exports=r},{"../helpers":1,"../options":4}],12:[function(e,t,n){"use strict";function r(e,t,n){return o.debug("Strategy: network only ["+e.url+"]",n),fetch(e)}var o=e("../helpers");t.exports=r},{"../helpers":1}],13:[function(e,t,n){"use strict";var r=e("./options"),o=e("./router"),i=e("./helpers"),c=e("./strategies"),s=e("./listeners");i.debug("Service Worker Toolbox is loading"),self.addEventListener("install",s.installListener),self.addEventListener("activate",s.activateListener),self.addEventListener("fetch",s.fetchListener),t.exports={networkOnly:c.networkOnly,networkFirst:c.networkFirst,cacheOnly:c.cacheOnly,cacheFirst:c.cacheFirst,fastest:c.fastest,router:o,options:r,cache:i.cache,uncache:i.uncache,precache:i.precache}},{"./helpers":1,"./listeners":3,"./options":4,"./router":6,"./strategies":10}],14:[function(e,t,n){t.exports=Array.isArray||function(e){return"[object Array]"==Object.prototype.toString.call(e)}},{}],15:[function(e,t,n){function r(e,t){for(var n,r=[],o=0,i=0,c="",s=t&&t.delimiter||"/";null!=(n=x.exec(e));){var f=n[0],h=n[1],p=n.index;if(c+=e.slice(i,p),i=p+f.length,h)c+=h[1];else{var l=e[i],d=n[2],m=n[3],g=n[4],v=n[5],w=n[6],y=n[7];c&&(r.push(c),c="");var b=null!=d&&null!=l&&l!==d,E="+"===w||"*"===w,R="?"===w||"*"===w,k=n[2]||s,$=g||v;r.push({name:m||o++,prefix:d||"",delimiter:k,optional:R,repeat:E,partial:b,asterisk:!!y,pattern:$?u($):y?".*":"[^"+a(k)+"]+?"})}}return i<e.length&&(c+=e.substr(i)),c&&r.push(c),r}function o(e,t){return s(r(e,t))}function i(e){return encodeURI(e).replace(/[\/?#]/g,function(e){return"%"+e.charCodeAt(0).toString(16).toUpperCase()})}function c(e){return encodeURI(e).replace(/[?#]/g,function(e){return"%"+e.charCodeAt(0).toString(16).toUpperCase()})}function s(e){for(var t=new Array(e.length),n=0;n<e.length;n++)"object"==typeof e[n]&&(t[n]=new RegExp("^(?:"+e[n].pattern+")$"));return function(n,r){for(var o="",s=n||{},a=r||{},u=a.pretty?i:encodeURIComponent,f=0;f<e.length;f++){var h=e[f];if("string"!=typeof h){var p,l=s[h.name];if(null==l){if(h.optional){h.partial&&(o+=h.prefix);continue}throw new TypeError('Expected "'+h.name+'" to be defined')}if(v(l)){if(!h.repeat)throw new TypeError('Expected "'+h.name+'" to not repeat, but received `'+JSON.stringify(l)+"`");if(0===l.length){if(h.optional)continue;throw new TypeError('Expected "'+h.name+'" to not be empty')}for(var d=0;d<l.length;d++){if(p=u(l[d]),!t[f].test(p))throw new TypeError('Expected all "'+h.name+'" to match "'+h.pattern+'", but received `'+JSON.stringify(p)+"`");o+=(0===d?h.prefix:h.delimiter)+p}}else{if(p=h.asterisk?c(l):u(l),!t[f].test(p))throw new TypeError('Expected "'+h.name+'" to match "'+h.pattern+'", but received "'+p+'"');o+=h.prefix+p}}else o+=h}return o}}function a(e){return e.replace(/([.+*?=^!:${}()[\]|\/\\])/g,"\\$1")}function u(e){return e.replace(/([=!:$\/()])/g,"\\$1")}function f(e,t){return e.keys=t,e}function h(e){return e.sensitive?"":"i"}function p(e,t){var n=e.source.match(/\((?!\?)/g);if(n)for(var r=0;r<n.length;r++)t.push({name:r,prefix:null,delimiter:null,optional:!1,repeat:!1,partial:!1,asterisk:!1,pattern:null});return f(e,t)}function l(e,t,n){for(var r=[],o=0;o<e.length;o++)r.push(g(e[o],t,n).source);var i=new RegExp("(?:"+r.join("|")+")",h(n));return f(i,t)}function d(e,t,n){return m(r(e,n),t,n)}function m(e,t,n){v(t)||(n=t||n,t=[]),n=n||{};for(var r=n.strict,o=n.end!==!1,i="",c=0;c<e.length;c++){var s=e[c];if("string"==typeof s)i+=a(s);else{var u=a(s.prefix),p="(?:"+s.pattern+")";t.push(s),s.repeat&&(p+="(?:"+u+p+")*"),p=s.optional?s.partial?u+"("+p+")?":"(?:"+u+"("+p+"))?":u+"("+p+")",i+=p}}var l=a(n.delimiter||"/"),d=i.slice(-l.length)===l;return r||(i=(d?i.slice(0,-l.length):i)+"(?:"+l+"(?=$))?"),i+=o?"$":r&&d?"":"(?="+l+"|$)",f(new RegExp("^"+i,h(n)),t)}function g(e,t,n){return v(t)||(n=t||n,t=[]),n=n||{},e instanceof RegExp?p(e,t):v(e)?l(e,t,n):d(e,t,n)}var v=e("isarray");t.exports=g,t.exports.parse=r,t.exports.compile=o,t.exports.tokensToFunction=s,t.exports.tokensToRegExp=m;var x=new RegExp(["(\\\\.)","([\\/.])?(?:(?:\\:(\\w+)(?:\\(((?:\\\\.|[^\\\\()])+)\\))?|\\(((?:\\\\.|[^\\\\()])+)\\))([+*?])?|(\\*))"].join("|"),"g")},{isarray:14}],16:[function(e,t,n){!function(){var e=Cache.prototype.addAll,t=navigator.userAgent.match(/(Firefox|Chrome)\/(\d+\.)/);if(t)var n=t[1],r=parseInt(t[2]);e&&(!t||"Firefox"===n&&r>=46||"Chrome"===n&&r>=50)||(Cache.prototype.addAll=function(e){function t(e){this.name="NetworkError",this.code=19,this.message=e}var n=this;return t.prototype=Object.create(Error.prototype),Promise.resolve().then(function(){if(arguments.length<1)throw new TypeError;return e=e.map(function(e){return e instanceof Request?e:String(e)}),Promise.all(e.map(function(e){"string"==typeof e&&(e=new Request(e));var n=new URL(e.url).protocol;if("http:"!==n&&"https:"!==n)throw new t("Invalid scheme");return fetch(e.clone())}))}).then(function(r){if(r.some(function(e){return!e.ok}))throw new t("Incorrect response status");return Promise.all(r.map(function(t,r){return n.put(e[r],t)}))}).then(function(){})},Cache.prototype.add=function(e){return this.addAll([e])})}()},{}]},{},[13])(13)});


// *** End of auto-included sw-toolbox code. ***



// Runtime cache 配置转换后的 toolbox 代码.

toolbox.router.get("/*", toolbox.cacheFirst, {"origin":"unpkg.com"});
toolbox.router.get("/npm/*", toolbox.cacheFirst, {"origin":"cdn.jsdelivr.net"});





/* eslint-enable */
