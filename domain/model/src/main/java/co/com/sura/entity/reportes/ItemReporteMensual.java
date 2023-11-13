package co.com.sura.entity.reportes;

import com.fasterxml.jackson.annotation.JsonPropertyOrder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.SuperBuilder;

@Getter
@Setter
@NoArgsConstructor
@SuperBuilder(toBuilder = true)
@JsonPropertyOrder({"dia"})
public class ItemReporteMensual extends ItemReporte {
    private String dia;
}
