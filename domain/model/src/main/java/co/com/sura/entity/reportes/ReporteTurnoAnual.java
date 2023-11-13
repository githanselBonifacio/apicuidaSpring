package co.com.sura.entity.reportes;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.SuperBuilder;

import java.time.Month;
import java.time.format.TextStyle;
import java.util.List;
import java.util.Locale;
import java.util.concurrent.atomic.AtomicReference;
import java.util.stream.Collectors;

@Getter
@Setter
@NoArgsConstructor
@SuperBuilder(toBuilder = true)
public class ReporteTurnoAnual extends ReporteTurno{
    private List<ItemReporteAnual> reportes;

    public static ReporteTurnoAnual setNombreMes(ReporteTurnoAnual reporte){
        List<ItemReporteAnual> itemsReportes = reporte.getReportes()
                .stream()
                .peek(itemReporteAnual -> {
                    int numeroMes = Integer.parseInt(itemReporteAnual.getMes());
                    String nombreMes = Month.of(numeroMes)
                            .getDisplayName(TextStyle.FULL, new Locale("es", "ES"));
                    itemReporteAnual.setMes(nombreMes);
                })
                .collect(Collectors.toList());
        reporte.setReportes(itemsReportes);
        return reporte;
    }
    public static ReporteTurnoAnual calcularResumen(ReporteTurnoAnual reporte){

        AtomicReference<Double> capacidadPromedio = new AtomicReference<>(0.0);
        AtomicReference<Double> totalHoras = new AtomicReference<>(0.0);
        AtomicReference<Integer> totalCitasCompletadas = new AtomicReference<>(0);
        AtomicReference<Integer> totalCitasCanceladas = new AtomicReference<>(0);
        AtomicReference<Double> cumpliminentoPromedio = new AtomicReference<>(0.0);

        reporte.getReportes().forEach(itemReporteAnual -> {
            capacidadPromedio.updateAndGet(v -> v + itemReporteAnual.getCapacidadPromedio());
            totalHoras.updateAndGet(v -> v + itemReporteAnual.getTotalHorasAtencionesCompletadas());
            totalCitasCompletadas.updateAndGet(v -> v + itemReporteAnual.getTotalCitasCompletadas());
            totalCitasCanceladas.updateAndGet(v -> v + itemReporteAnual.getTotalCitasCanceladas());
            cumpliminentoPromedio.updateAndGet(v -> v + itemReporteAnual.getCumplimientoCitasPromedio());
        });
        ResumenReporteTurno resumen = ResumenReporteTurno
                .builder()
                .capacidadPromedio(capacidadPromedio.get()/reporte.getReportes().size())
                .totalHorasAtencionesCompletadas(totalHoras.get())
                .totaCitasCompletadas(totalCitasCompletadas.get())
                .totalCitasCanceladas(totalCitasCanceladas.get())
                .cumpliminetoCitasPromedio(cumpliminentoPromedio.get()/reporte.getReportes().size())
                .build();

        reporte.setResumen(resumen);

        return reporte;
    }
}
