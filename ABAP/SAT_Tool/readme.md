for my SAP community blog [A compare tool: Download and analyze the runtime performance result from SAT](https://blogs.sap.com/2013/11/21/a-compare-tool-download-and-analyze-the-runtime-performance-result-from-sat/)

# 2018-05-17

11:08AM if task type is set as * in ST12, no actual content is stored in trace?!

11:17AM still does not work even if I select "ALL Server", 难道因为没有http变化，所以就没有trace？那我做一些navigation试试。

11:28AM try X3C. 根本没装。
11:35AM AG3选了TPM—PRO之后都需要等很久。进去之后选择Product二级菜单需要10秒。
12:04PM caused by 302 redirect, but why?! 执行到redirect断点停下来时，已经很快了。

WorkAreaViewSet.htm-_ONLAYOUT: 1 second

handler class: CL_HTTP_EXT_BSP~HANDLE_REQUEST, end: CL_HTTP_RESPONSE~REDIRECT

PROD_ALL load 需要4.2秒