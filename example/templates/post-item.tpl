<apply template="default">
  <li>
    <bind tag="url"><hakyll field="url"/></bind>
    <a href="${url}"><hakyll field="title"/></a> - <hakyll field="date"/>
  </li>
</apply>
