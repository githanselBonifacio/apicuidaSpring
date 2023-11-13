package co.com.sura.entity.reportes;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder(toBuilder = true)
public class ResumenReporteTurno {
    private Double capacidadPromedio;
    private Double totalHorasAtencionesCompletadas;
    private Integer totaCitasCompletadas;
    private Integer totalCitasCanceladas;
    private Double cumpliminetoCitasPromedio;
 }
