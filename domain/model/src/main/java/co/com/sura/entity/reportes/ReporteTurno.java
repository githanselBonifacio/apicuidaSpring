package co.com.sura.entity.reportes;

import co.com.sura.entity.maestro.Regional;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.SuperBuilder;

@Getter
@Setter
@NoArgsConstructor
@SuperBuilder(toBuilder = true)
public class ReporteTurno {
    private Regional regional;
    private ResumenReporteTurno resumen;
}
