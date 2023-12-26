package co.com.sura.personal.entity;

import co.com.sura.maestros.entity.HorarioTurno;
import co.com.sura.remision.factory.RemisionFactory;
import co.com.sura.remision.entity.datosremision.ItemDiaTurno;
import co.com.sura.remision.entity.datosremision.ItemSecuenciaTurno;
import lombok.*;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;

@Data
@AllArgsConstructor
@NoArgsConstructor
@ToString
@Builder(toBuilder = true)
public class SecuenciaTurno {
    private String nombre;
    private String descripcion;
    private List<ItemDiaTurno> itemsDiaTurno;

    public static SecuenciaTurno agruparItemsDiaTurno(List<SecuenciaTurno> listSecuenciaTurno){
        SecuenciaTurno secuenciaTurno = listSecuenciaTurno.get(0);
        ArrayList<ItemDiaTurno> itemsDiaTurno = new ArrayList<>();
        listSecuenciaTurno
                .stream()
                .flatMap(st -> st.getItemsDiaTurno().stream())
                .distinct()
                .sorted(Comparator.comparing(ItemDiaTurno::getNumeroDia))
                .collect(Collectors.groupingBy(ItemDiaTurno::getNumeroDia))
                .forEach((numeroDia, items) -> {
                    List<HorarioTurno> horariosTurno = items.stream()
                            .flatMap(item -> item.getHorariosTurno().stream())
                            .sorted(Comparator.comparing(HorarioTurno::getHoraInicio))
                            .collect(Collectors.toList());
                    ItemDiaTurno itemDiaTurno = RemisionFactory
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
