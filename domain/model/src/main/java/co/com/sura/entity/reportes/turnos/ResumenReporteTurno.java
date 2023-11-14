package co.com.sura.entity.reportes.turnos;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder(toBuilder = true)
public class ResumenReporteTurno {
    @JsonProperty("capacidadPromedio")
    private double capacidad;

    @JsonProperty("totalHorasAtencionesCompletadas")
    private double totalHoras;

    @JsonProperty("totalCitasCompletadas")
    private int citasCompletadas;

    @JsonProperty("totalCitasCanceladas")
    private int citasCanceladas;

    private int totalRemisiones;
    private int totalNovedades;

    @JsonProperty("cumpliminetoCitasPromedio")
    private double cumplimineto;
 }
