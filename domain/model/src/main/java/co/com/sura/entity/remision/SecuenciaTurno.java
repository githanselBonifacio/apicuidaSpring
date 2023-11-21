package co.com.sura.entity.remision;

import co.com.sura.entity.maestro.HorarioTurno;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder(toBuilder = true)
public class SecuenciaTurno {
    private String nombre;
    private String descripcion;
    private List<ItemDiaTurno> itemsDiaTurno;

    public static SecuenciaTurno agruparItemsDiaTurno(List<SecuenciaTurno> listSecuenciaTurno){
        var secuenciaTurno = listSecuenciaTurno.get(0);
        var itemsDiaTurno = new ArrayList<ItemDiaTurno>();
        listSecuenciaTurno
                .stream()
                .flatMap(st -> st.getItemsDiaTurno().stream())
                .distinct()
                .sorted(Comparator.comparing(ItemDiaTurno::getNumeroDia))
                .collect(Collectors.groupingBy(ItemDiaTurno::getNumeroDia))
                .forEach((numeroDia, items) -> {
                    var horariosTurno = items.stream()
                            .flatMap(item -> item.getHorariosTurno().stream())
                            .sorted(Comparator.comparing(HorarioTurno::getHoraInicio))
                            .collect(Collectors.toList());
                    var itemDiaTurno = RemisionFactory
                            .crearItemDiaTurno(numeroDia,items.get(0).getNombreDia(),horariosTurno);
                    itemsDiaTurno.add(itemDiaTurno);
                });
        secuenciaTurno.setItemsDiaTurno(itemsDiaTurno);
        return secuenciaTurno;
    }
    public static  SecuenciaTurno crearSecuenciaTurnoFromItemsSecuencia(ItemSecuenciaTurno itemSecuenciaTurno){
        var itemsDiaTurno = RemisionFactory.crearItemDiaTurno(itemSecuenciaTurno);
        return RemisionFactory.crearSecuenciaTurno(itemSecuenciaTurno,itemsDiaTurno);
    }
}
