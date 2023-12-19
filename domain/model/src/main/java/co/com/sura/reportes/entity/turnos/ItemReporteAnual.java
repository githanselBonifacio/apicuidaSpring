package co.com.sura.reportes.entity.turnos;

import com.fasterxml.jackson.annotation.JsonPropertyOrder;
import lombok.*;
import lombok.experimental.SuperBuilder;

import java.time.Month;
import java.time.format.TextStyle;
import java.util.Locale;

@Getter
@Setter
@Data
@EqualsAndHashCode(callSuper = true)
@NoArgsConstructor
@ToString
@SuperBuilder(toBuilder = true)
@JsonPropertyOrder({"mes"})
public class ItemReporteAnual extends ItemReporte{
    private String mes;

    public static ItemReporteAnual converNumeroToNombreMes(ItemReporteAnual itemReporteAnual){
        int numeroMes = Integer.parseInt(itemReporteAnual.getMes());
        String nombreMes = Month.of(numeroMes)
                .getDisplayName(TextStyle.FULL, new Locale("es", "ES"));
        itemReporteAnual.setMes(nombreMes);
        return itemReporteAnual;
    }

}
