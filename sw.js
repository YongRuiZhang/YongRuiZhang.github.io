/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","13498f694777bd050cee4a3c5015c7c5"],["/about/index.html","30a13bdcc2291731ee6947dd68ac76fd"],["/archives/2023/01/index.html","9ddb91eec5c53bd11c220a3995ab2766"],["/archives/2023/02/index.html","6d4cc58499c5bd10bd941f84635a1980"],["/archives/2023/02/page/2/index.html","2104cbe8c2543226a837f3924f500e9c"],["/archives/2023/03/index.html","7c78dc8f1503144ad25d6559cbefdb52"],["/archives/2023/05/index.html","d2361b8bb208ed20059243dace62ffc2"],["/archives/2023/06/index.html","0ea59c54d67885d19143c5749fd160e7"],["/archives/2023/09/index.html","7f53e967e9b4bd3df790ae947f1377f0"],["/archives/2023/11/index.html","cb851efd43196e11b69227fee69be929"],["/archives/2023/12/index.html","517b62c6c2e0f5533cd970131ada7d3d"],["/archives/2023/index.html","0556e63c796a8cd52064deb4d201465c"],["/archives/2023/page/2/index.html","d77057a3b85ff7a8afd4b7232cfe41ae"],["/archives/2023/page/3/index.html","d38ddb8beba7d4c4b0c1a1fd8a9a766f"],["/archives/2023/page/4/index.html","5c711b200028930a05c900f992f39739"],["/archives/2024/02/index.html","a70ea33e0c98d6399d25458cb7041750"],["/archives/2024/index.html","4cd13bffb0b9e4e40b4fc0cfe4b99a18"],["/archives/index.html","304ea8233f801dba626df4aaf9aba503"],["/archives/page/2/index.html","93aa9b5e0890006b5c155a71e81ba6fb"],["/archives/page/3/index.html","695bf35c991cb5647e7440fc9c8f85d6"],["/archives/page/4/index.html","9de7f7c643766751086877ebcd371340"],["/baidu_verify_codeva-qQP2iZOMLX.html","4e21c0f68a66d9e0f751387c0f1cd032"],["/categories/Java/index.html","9882cf9b9900092f8ecf88db6c0ebaf6"],["/categories/Java/后端/index.html","03eb8f657cc6f911515239e81fa0dac3"],["/categories/Java/基础/index.html","fad2bdc843619f6feab0bd55b9f8baf6"],["/categories/Java/基础/集合/index.html","33b212e1ad476bce4b4e1571365c95ae"],["/categories/Python/index.html","5be9a1705504fdea80ade9acb36768bc"],["/categories/Python/编程环境/index.html","89a346021f855fd2f9bef4421802a125"],["/categories/R语言/index.html","c23e37c64c1c6bab33465a65b1a2035c"],["/categories/R语言/编程环境/index.html","5a98afedd4688e9f8507bd6cfe5a36be"],["/categories/index.html","63835c7e5e6487e39c222a58200244b5"],["/categories/中间件/index.html","258cf74076199f7dfc0b18b61197907b"],["/categories/前端/Vue/index.html","fc4a2e9bf5273b17e0ee21ee4eecd2ca"],["/categories/前端/index.html","dc67eff44857f4a67c347b0722b69864"],["/categories/大数据开发/ElasticSearch/index.html","3712e0f1e96d0ef5edfbc6a86c9adba0"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","c726ebcc9d4d114ab45b85b44ee11cd3"],["/categories/大数据开发/HBase/index.html","e4fb37f0bf0e539809db5cb5299681ec"],["/categories/大数据开发/HBase/学习笔记/index.html","7c2caf45da0480cbca4bd19f390b9ba4"],["/categories/大数据开发/HBase/环境搭建/index.html","af324db35beb6bb330300332c4502917"],["/categories/大数据开发/Hadoop/index.html","1662d4bcd288a2b941a270cffaf07b6c"],["/categories/大数据开发/Hadoop/技术/index.html","986150e700fcefd09d03714b879087b9"],["/categories/大数据开发/Hadoop/环境搭建/index.html","1ff1ebd5185d06620b3ac8be8e35784e"],["/categories/大数据开发/Redis/index.html","5cc2703f15168b4ef5173c696af14d86"],["/categories/大数据开发/Redis/技术/index.html","a463935eee4e7cab51393b0b6fc45dba"],["/categories/大数据开发/Redis/环境搭建/index.html","614adbd72d75a8e308a3bd5b7d7fed30"],["/categories/大数据开发/Spark/index.html","cadbda8a2235eafadbdcc97d64abca69"],["/categories/大数据开发/Spark/环境搭建/index.html","2d0ba041f9c0d5a619226802cfbefacb"],["/categories/大数据开发/Zookeeper/index.html","7f5e66ea54accde6194f3639921dfe7f"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","803e8f731f1708d94058262adbdc32b1"],["/categories/大数据开发/index.html","638f364d507e0b3a13dfad4c72fea12e"],["/categories/学校课程/index.html","cb35789e8a7c69b4f03392542e4da2eb"],["/categories/学校课程/计算机操作系统/index.html","c012894abe261ad88d021e5401869e4d"],["/categories/操作系统/Linux/index.html","b86900d81e640bd2d71a0dc6ea8555e1"],["/categories/操作系统/Mac/index.html","bd69df5598c3103671f3e12f6f7a15b2"],["/categories/操作系统/Windows/index.html","fa3682bf7707b808446218121fcea341"],["/categories/操作系统/index.html","cbc84aca5bf27cdfbc5e1abe3a094fd6"],["/categories/数学建模/index.html","5ade1eda3c5099ee13919ef22af17eb2"],["/categories/数学建模/latex/index.html","7e2f14a2777ff0f4bf1e3917fe66dccd"],["/categories/数学建模/优化类/index.html","c4a1d93489e8fcac9ada63953106f964"],["/categories/数学建模/优化类/现代优化算法/index.html","af44d6c8b34cac200ed342664155f459"],["/categories/数学建模/优化类/规划类/index.html","d2777b830ed451afc934b873bca375f4"],["/categories/数学建模/绘图/index.html","141fba9fa6527191e800d7bc873b559e"],["/categories/数据库/MySQL/index.html","f520d0c1a187dd4132f4e8a8fc98edd1"],["/categories/数据库/index.html","9c88f1795fd7a9b1858799c9586abbe2"],["/categories/数据结构和算法/index.html","81f838b3b130bb2fce7ce12ff359bdd9"],["/categories/数据结构和算法/page/2/index.html","909c0092af3564be05a8a88f2feaf715"],["/categories/数据结构和算法/基本原理/bfs/index.html","86b7dff843b7770d57224e30f69f6a11"],["/categories/数据结构和算法/基本原理/dfs/index.html","934aae7290da6197133612d3724ef7af"],["/categories/数据结构和算法/基本原理/index.html","acb221e61fa8ad8f36d569d387613055"],["/categories/数据结构和算法/基本原理/动态规划/index.html","407672332952001d298df383b1801808"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","fbf642e7b788fe5d321c6f4eab0aaf65"],["/categories/数据结构和算法/基本原理/图论/index.html","219f88b2618a8aebae7dde5bb77e72d2"],["/categories/数据结构和算法/基本原理/字符串/index.html","ee5edb258c59cefc2fec7d41d52e4bea"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","b89b36dca017416d5adc5ebbee59dd16"],["/categories/数据结构和算法/基本原理/数论/index.html","477b0dfd8676fefbe8e6fbccf1044928"],["/categories/数据结构和算法/基本原理/树论/index.html","07902825e0baa233f1a8702037ac5c22"],["/categories/数据结构和算法/基本原理/链表/index.html","68844a86d5998fc29491ea100804aa2d"],["/categories/数据结构和算法/算法题/index.html","6ef8099d5b16e123750b379c549dede8"],["/categories/数据结构和算法/算法题/二分查找/index.html","3678a4e727a31fd2a39da747c7dabb08"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","c4843f0604bd9e16a2856a2f5287a1f2"],["/categories/数据结构和算法/算法题/动态规划/index.html","d93154cfce719bbd62a080eb81335947"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","4280e60f475f6c58369b0f5ba56388e7"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","cf506eb5e11633a785d780f100e55b15"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","4ba698926140ea2822296241e4ee3133"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","5474a084870dd59e66d126d36749b7e1"],["/categories/数据结构和算法/算法题/数论/index.html","01323b265836749ba9acdca5eedd288c"],["/categories/数据结构和算法/算法题/栈和队列/index.html","1131ac7da5a4baa29c7d8c1fb316bcde"],["/categories/数据结构和算法/算法题/树论/index.html","83a9c0a393a27b4722b7e5e46a42b497"],["/categories/杂七杂八/index.html","2848781864e163bcf2065a2eb60610b5"],["/categories/杂七杂八/博客搭建/index.html","39db7bab748eec22c285a260970027a1"],["/categories/编程工具下载/index.html","c33e66f0f0476a64960f7d816554b40b"],["/categories/编程环境/index.html","745c8ffc332d5ad60175d89191cd4704"],["/categories/编程环境/大数据/index.html","ad136e83154d322c3446614e8c3bb2b7"],["/categories/英语学习/index.html","e2281798c7bea4f501ae6c4552860401"],["/categories/英语学习/英语语法/index.html","6913f3ecfe65954d067f68a040b63ed6"],["/comments/index.html","dc10e555a1b9a3b71ae8d0f89d4389c0"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/background.css","ac15c4d880156c18b725de616496838e"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/twikoo2.css","0577747a2a9555baefc708447feabfad"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","16e173985d136ced4a6bef304fd87c10"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","67e3035803c66a55a191bf3b78db9dca"],["/movies/index.html","911517fd13c2e3243fc2341bbc422955"],["/music/index.html","fe0a2b095ae1a994b735c7a5075fd30e"],["/page/2/index.html","39070e1324317d52c4fb7cb8c686d78a"],["/page/3/index.html","b747b359fc3a2720c7cdffe8892ec859"],["/page/4/index.html","0a2aaeb1eb5a396506495afb7fdbf289"],["/page/5/index.html","c520a9b39e5ad008d16cecff551dbb75"],["/page/6/index.html","6e4894a780b121770347c6a7b1589825"],["/posts/1021360842.html","6cd4e1607a48850d2213192b572a04ec"],["/posts/1120620192.html","41cf22c78f1736c51dde04f682203368"],["/posts/1141628095.html","def56763cde622538e736c790f0150f1"],["/posts/1168613674.html","f50558c8e33f0638a25db7a4d0d05b90"],["/posts/1219920510.html","04bcc4d64264bdebfd3d7e221c49b2aa"],["/posts/1222166338.html","13dbca6b6572e527826d7d5c6557ed74"],["/posts/1259097482.html","0b3b55bd788f0647ce709fa6c29532d4"],["/posts/1271036369.html","ec6c631aa3a91b8fc1fd0b4e4abdf88c"],["/posts/1312847445.html","262b0ce029a4b4ba2c0c966b4c561f66"],["/posts/135355774.html","69d60cd23e016eee82992938a0abc38e"],["/posts/1375344716.html","c1e6d9c0b5e2cf6b2deed53344dc9008"],["/posts/1388991698.html","979bb1200b1520b168472e5557d0e80b"],["/posts/1410315814.html","18b1e7a3e9d4466963009939314573f2"],["/posts/1452790229.html","e9b8ac20f3b162debe1420df8723af93"],["/posts/1470079884.html","9c0819ac52133a84a8622350b90f414f"],["/posts/1470079885.html","e39af5c3d4de8a7085b14f7781c0926a"],["/posts/1470079886.html","894d9e5a56a45490f75be197bc7b0925"],["/posts/1470079887.html","97551d19257ef15589c12ef677d2f6f0"],["/posts/1498536549.html","b82179bf7258afa5bb2a3fefb56a1bc5"],["/posts/1539568593.html","23bdb8799e3025d6507fc85a38c80cf0"],["/posts/1547067935.html","b4e9a857af12229db3517a8185a12d14"],["/posts/1557866301.html","9cb9d4e486be6609e51e75d0598a859f"],["/posts/1571776361.html","6a12a8baf66033f3fe7af82616663a3f"],["/posts/1605124548.html","f8e1dd311bc89aea9c64e315daaec2ba"],["/posts/1633036852.html","a20cae720afec04a98b7ec4c3abab95c"],["/posts/1674202625.html","c26148f20ca26b43acca21afdde9a2e0"],["/posts/1765123828.html","33da9a0172d896d86d03db83722fe276"],["/posts/1767336200.html","97986a1f192cdc53b2545c4d734d8a75"],["/posts/1776114197.html","304dde9f84ce42a1ba330b619772b3f9"],["/posts/1817748743.html","68c6955341287ca3c4d0927cee61e81b"],["/posts/1925125395.html","f520b58855028549d70ce927f27ffc96"],["/posts/1966191251.html","0072ac5e4e1811ee3460a6fb817d8f85"],["/posts/1987617322.html","7b9107813046fb826fcd48a359bcb677"],["/posts/1999788039.html","35350153cb723529eb4230092d9a3020"],["/posts/2075104059.html","7aa3fca6baf7470e139b3e664058b6b6"],["/posts/2087796737.html","3e479d5a54885bb88ffba64aef899080"],["/posts/2106547339.html","df4527153a06673f261f7ed9e2871d2e"],["/posts/2207806286.html","36006f3759c7bd0ad73f28fabbc82038"],["/posts/2225903441.html","5f8352b2b2903a807e7acbefe55cbb72"],["/posts/2265610284.html","b0b3bc2f718967647268721d58f08f02"],["/posts/2281352001.html","0c1d92ff1557403465c619c3f23efdc3"],["/posts/2364755265.html","b342ddb0802752fd29e550e255e1b24b"],["/posts/2414116852.html","19ac73e731b55b4f391a1ba264b5d80b"],["/posts/2421785022.html","a9ecf65a26426923bf6bf5e1e4f8a610"],["/posts/2482902029.html","5c266a847b4ba59476279cf783a88b50"],["/posts/2495386210.html","a9104d55185df9e06107145db850823e"],["/posts/2516528882.html","2a2317763f8e0603406d41a0226f27cd"],["/posts/2526659543.html","dbb437f7ed4065caf0568b283583cafb"],["/posts/2529807823.html","1373f76ed8ed79dd10251f06158fd51b"],["/posts/2596601004.html","f29fa4eb2e1f40157b0f5bf08a62430b"],["/posts/2697614349.html","c330c89057c99eedfb41f39939cd6e51"],["/posts/2742438348.html","26c6f028c9b86c7f82ce404a22444909"],["/posts/2768249503.html","5a5c22ebaa5ce60e4090d1bb0825798d"],["/posts/2864584994.html","2765f13eadf3fe331d77834116b37a47"],["/posts/2888309600.html","919f73a32550734f3f7132bb75e64039"],["/posts/2891591958.html","4d89af251cde269b1704adee5921aaac"],["/posts/2909934084.html","6cd24281042b779a7c1182f449c10c76"],["/posts/2920256992.html","79ed663daddd901154aee5d0a2a6ec17"],["/posts/2959474469.html","5cfb76a9669faad428c39a8d1087095d"],["/posts/3005926051.html","60951c7cf56e4767966652a2787dbef9"],["/posts/309775400.html","2a24a48e21436a2b7bfa4ff1e2dcad6e"],["/posts/3156194925.html","5bacc523a1b30dbb70d64bb4d5c7b3a3"],["/posts/3169224211.html","1cf6a55b6d405190d320c4dede388e7b"],["/posts/3213899550.html","8765a060deb8f477233a0daa4c73f53e"],["/posts/3259212833.html","a785ad95905f0d675b89b99d5a376fe1"],["/posts/3266130344.html","83afa54493b30942d209147b7e3d1fa3"],["/posts/3292663995.html","11be809f831e78bb33f3b111eddf0a10"],["/posts/3297135020.html","2170851b00ae2d1ea72993e5c42fdafb"],["/posts/3306641566.html","996c38c1381fd0bfb7825c09e287a766"],["/posts/3312011324.html","0f3aaf368b8a913e740f4814f6bf5f48"],["/posts/336911618.html","eda9c5990f67dcf1847cf13c4a07fc75"],["/posts/3402121571.html","8543f96e451d695cbbdae7b0f5141216"],["/posts/3405577485.html","3744bde3aac728de44fe60b82b706081"],["/posts/3498516849.html","405b1d3546b0b49d0f0dff0b0cc3f0f4"],["/posts/3513711414.html","adc7e2133d5ef868a087830cf2d317c2"],["/posts/3523095624.html","610f7e2db6557045249821c0ffd6e80c"],["/posts/3546711884.html","39c6c252675729fdc389ad36fa34b46a"],["/posts/3731385230.html","60a9565eee36c904ff13b82385410d11"],["/posts/3772089482.html","9a887b04e8eca40cfeed6133cd0c3d14"],["/posts/386609427.html","6fdf67453c294667cfa51806398141cc"],["/posts/4044235327.html","f1917ad6dae14ac9929c841b80eec875"],["/posts/4115971639.html","6671fb95400ec8147f764f12950a75ab"],["/posts/4130790367.html","169e23057d87182989d96f297a6d1f8c"],["/posts/4131986683.html","17ab595d21691301102b89fe9007be90"],["/posts/4177218757.html","e742defcdb0da90c722314b7516b8674"],["/posts/4192183953.html","a4f9a0a2a0db830d8fa5235bdd304a21"],["/posts/4261103898.html","e7b10ef0b488c6ac51b7507fcf314560"],["/posts/469711973.html","0b8221b699cced11a88b305779932b41"],["/posts/482495853.html","643b4db764c0f0e53f5d88dc6cf25f34"],["/posts/488247922.html","23abb5daacc0c68351296fed8fa91851"],["/posts/517302816.html","433b85d22cf7c4b3401879e3ad5d2bb5"],["/posts/570165348.html","cf80ccad371fae3651d3dc8357bf6489"],["/posts/595890772.html","10563c17e0cadb000d454023942fc228"],["/posts/67485572.html","12992b71af2c446856130239213d190c"],["/posts/694347442.html","f48d6843130fc7671751672f9f626c6a"],["/posts/707384687.html","e55a4aaaad9474a284f5cdbc6267ef6c"],["/posts/71180092.html","ef2f30b83ec3ec36a8ac10402cffd236"],["/posts/716459272.html","d76b14b0f09cf1eb481ef04cccb37658"],["/posts/765481613.html","b7c37bb65f8ca23761e746c9c7d8a32a"],["/posts/778231993.html","1004ed04d66ff4de8625fa090e97afa3"],["/posts/795397410.html","c12a998faa6266e5f0f98c148d8c9fc1"],["/posts/820223701.html","b4d3b1a957a668a644b1f66053b34b67"],["/posts/830372185.html","f4522e3f970e742d924437457c0987c9"],["/posts/88294277.html","8663157428236c7d56d7a4bfe1854acd"],["/posts/939963535.html","e78b2a4e56c483c6093b60cdb6602bb0"],["/posts/983786067.html","5b6e7985d352840230f9fdab2d97318f"],["/sw-register.js","ab7a7b240b8a088fd33ad68c48c0d03c"],["/tags/C/index.html","ced48dbaefb81796ff42752d79d6c884"],["/tags/C/page/2/index.html","068925a3bb51b95c9b44bfd44c81c054"],["/tags/C/page/3/index.html","e3d290ced0145db78c2ca0fdc5cd34a4"],["/tags/C/page/4/index.html","97ec18762c22040ba910725d506ed149"],["/tags/ETL/index.html","a13619bec6cdf38016f66e26ed57736e"],["/tags/ElasticSearch/index.html","879d49a77808ade390863083387e91d7"],["/tags/GUI/index.html","0ec5dd497ec406b6a24c443a35fca888"],["/tags/HBase/index.html","fece1e0c80a69f1fe89681544c72ca69"],["/tags/Hadoop/index.html","e03a424a8d874d615233445ba244a724"],["/tags/Hadoop/page/2/index.html","18aadd132c3c0ad147c31ecf34de6b92"],["/tags/Java/index.html","5319a79121a2c1875ec5cec4115769a2"],["/tags/Java后端/index.html","9d40f873634d07d517a59c006afa08cb"],["/tags/Java后端/page/2/index.html","dd5b3ce8a2474a11e1c1dbff4ea059ea"],["/tags/Java基础/index.html","af81c5210b5c9e2bd6fb73b5987b8633"],["/tags/Java基础/page/2/index.html","d78859ed9401e1a3128850540150dac7"],["/tags/Kettle/index.html","22f13f4795d7f6384cfc263ce0f50adf"],["/tags/Kibana/index.html","04d15ba20d53e3d1280a7182891dd375"],["/tags/Linux/index.html","54c20705c4eebccf33a096818589a29c"],["/tags/Linux/page/2/index.html","a3d560641ce34b1dd9d9837cf6558598"],["/tags/Linux/page/3/index.html","0a47187703820e6544caa1e084bdc7db"],["/tags/Mac/index.html","587f19516928ddfddb484a1a20783c65"],["/tags/Mac/page/2/index.html","edcc1a918ddf2f7cda93f6778f1f95b9"],["/tags/Maven/index.html","99581e02f52c3e2a8fa7e8e3971b94ff"],["/tags/MySQL/index.html","74bc0f618868047475df47d7d0ef4aaf"],["/tags/Python/index.html","fcafd01e80682f49e43ed17eaf4c9593"],["/tags/Redis/index.html","6f07d99081cda1514f949678ea05ce91"],["/tags/R语言/index.html","3fcfe1b2144199eb714fb3a8e6555b15"],["/tags/Spark/index.html","b81386e90f72faa7d53dcd4d3ed91c7e"],["/tags/Ubuntu/index.html","397939f88a46e4a621f75667f13f4649"],["/tags/Vue/index.html","76d75e4c70e06f83f6ff2e120fbdc156"],["/tags/Windows/index.html","e483f0173a8ed8fbc96244a6f408104d"],["/tags/ZooKeeper/index.html","e6470c0119ef630099dfad1a6fe8dd5e"],["/tags/bfs/index.html","eeeb124a7b6e96a0fcd205cc41a0bb95"],["/tags/dfs/index.html","f6844fb8c240d709741bdf4d2fc453a4"],["/tags/folium/index.html","e1a5ccd509d6e70689263df240fccaa2"],["/tags/git/index.html","949f08acbf9c2af293203f18e9322b56"],["/tags/index.html","6b02f11b7bc87fb913c7e4208cf5284c"],["/tags/latex/index.html","5e3700049eb376e4d248c7183b7e4f82"],["/tags/中间件/index.html","4c40d4bc9655c4a226e05df9f2665382"],["/tags/二分查找/index.html","473bb03182da5f45720d1c6c279faa33"],["/tags/优化类/index.html","1da855689feaa1a4bf21bb842d2e054e"],["/tags/前端/index.html","34f10f9898cd0d05ca79c9188831a291"],["/tags/前缀和与差分/index.html","62af314fadb570ebebdcf026b857d30d"],["/tags/动态规划/index.html","7895256c30d5e6e6ac03a53d79a5e5c7"],["/tags/动态规划/page/2/index.html","47e1ff38116cb3e534498de0975cfaae"],["/tags/博客搭建/index.html","12609faf0823c099df1acad7b567485d"],["/tags/图论/index.html","55a6de24b1f5c598465278efd818c43f"],["/tags/大数据/index.html","a7151e65f506559b208f343e1d89a09b"],["/tags/大数据/page/2/index.html","40faefc71a7fff0325b83321818aedae"],["/tags/操作系统/index.html","8bef22f935edb4201bb9d55ddc7babdb"],["/tags/数学建模/index.html","8e0986d80ad413b245b3a9c6001f4eea"],["/tags/数据库/index.html","193ea61240e7127c308903ba17a940c0"],["/tags/数据结构和算法/index.html","c8b4d5338f339353ac8297c62b3308de"],["/tags/数据结构和算法/page/2/index.html","8fa5d335a2189055e33aaa69e7891641"],["/tags/数据结构和算法/page/3/index.html","e4e2c7b6ae695351f92f600e0dc26b50"],["/tags/数据结构和算法/page/4/index.html","0d348a6cd04fd8a5eff54d5e95c8aac7"],["/tags/数组和字符串/index.html","c67428116b3d0c3d48f16f6d78e6c2f3"],["/tags/数论/index.html","66a699bf2678ec5d1416be0d169f4815"],["/tags/枚举类/index.html","dace22dc268442acb5d77648556c26cf"],["/tags/栈和队列/index.html","56d543ff67a6bf5a6612d2be0e09dcec"],["/tags/树论/index.html","92b103b21dbdbf2efb942312ed164af5"],["/tags/测试/index.html","38efd5e8d175db2819653b4e35ba2928"],["/tags/环境/index.html","832e64008d490208443bd6296444dc74"],["/tags/环境变量/index.html","4b719af17b82fd62fafc23f0b853c57d"],["/tags/绘图/index.html","7943b070f46e52299060331a8380e99f"],["/tags/编程工具/index.html","ce8316558b4179dff28f592b16cf1e0c"],["/tags/编程环境/index.html","5b926341e1a1be4661ed5d7eeefb3239"],["/tags/网络编程/index.html","88e9309024fb155106dc509ff6e4c3a6"],["/tags/英语语法/index.html","4681748d94c9f321e9dc4c3a88513e77"],["/tags/计算机操作系统/index.html","9a6439e241037df1ba4890e313a5c58d"],["/tags/论文/index.html","8134835572038f074f81f6001e6e787a"],["/tags/资源下载/index.html","751d17faec802f28b84c33ad1ec34a26"],["/tags/链表/index.html","2f4dace5528558dad8a9494df5a14c35"],["/tags/集合/index.html","5bec08c9705251c486b04e476d090b1b"],["/tags/集群/index.html","dc8bc898dcac9102cf3588051c8dfe26"]];
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
