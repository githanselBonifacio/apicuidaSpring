package co.com.sura.reportes.entity.turnos;

import co.com.sura.maestros.entity.Regional;
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
