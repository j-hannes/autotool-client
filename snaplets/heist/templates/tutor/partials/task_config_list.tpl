<form>
  <table class="table table-bordered table-striped">
    <thead>
      <tr>
        <th>Name</th>
        <th>Aufgabentyp</th>
        <th>Erstellt</th>
        <th class="center">Zugewiesen</th>
        <th class="center">Loeschen</th>
      </tr>
    </thead>
    <tbody>
      <tasks>
        <tr>
          <td>
            <!--<a href="#taskConfigDetailsModal_${taskConfigId}">-->
              <taskConfigName />
            <!--</a>-->
          </td>
          <td><taskType /></td>
          <td><dateCreated /></td>
          <td class="center"><assignments /></td>
          <td class="center">
            <a href="#" class="btn btn-small btn-danger">
              <i class="icon-remove"></i>
            </a>
          </td>
        </tr>
      </tasks>
    </tbody>
  </table>
</form>
