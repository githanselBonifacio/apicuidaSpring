package co.com.sura.entity.reportes.turnos;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.SuperBuilder;


@Getter
@Setter
@NoArgsConstructor
@SuperBuilder(toBuilder = true)
public class ItemReporte {
    private Double capacidadPromedio;
    private Double totalHorasAtencionesCompletadas;
    private Integer totalCitasCompletadas;
    private Integer totalCitasCanceladas;
    private Integer totalRemisiones;
    private Integer totalNovedades;
    private Double cumplimientoCitasPromedio;
}
