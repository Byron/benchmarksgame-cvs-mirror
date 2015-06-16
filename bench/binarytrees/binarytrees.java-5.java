/**
 * The Computer Language Benchmarks Game
 * http://benchmarksgame.alioth.debian.org/
 *
 * Loosely based on Jarkko Miettinen's implementation. Requires Java 8.
 *
 * contributed by Heikki Salokanto.
 * modified by Chandra Sekar
 */

import java.util.LinkedList;
import java.util.Queue;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;

public class binarytrees {

    // In case you have a hyper-threaded 2-core CPU, you should adjust this to THREADS = 2.
    // There is no easy way to reliably detect if HT is enabled.
    static final int PROCESSORS = Runtime.getRuntime().availableProcessors();
    static final int THREADS = PROCESSORS > 6 ? 6 : PROCESSORS;

    public static void main(String[] args) throws Exception {
        int n = args.length > 0 ? Integer.parseInt(args[0]) : 0;
        int maxDepth = (6 > n) ? 6 : n;
        ExecutorService executorService = Executors.newFixedThreadPool(THREADS);
        Queue<Future<?>> tasks = createTasks(executorService, maxDepth, maxDepth + 1);

        System.out.println("stretch tree of depth " + (maxDepth + 1) + "\t check: " + tasks.poll().get());

        // Wait for the "Long-lived tree" to finish building, and preserve it in memory
        TreeNode longLivedTree = (TreeNode) tasks.poll().get();

        for (Future<?> future : tasks)
            System.out.println(future.get());

        System.out.println("long lived tree of depth " + maxDepth + "\t check: " + longLivedTree.check());

        executorService.shutdown();
    }

    static Queue<Future<?>> createTasks(ExecutorService executor, int maxDepth, int stretchDepth) {
        Queue<Future<?>> list = new LinkedList<>();

        // "stretch memory" task
        list.add(executor.submit(() -> TreeNode.create(0, stretchDepth).check()));

        // "long-lived tree" task
        list.add(executor.submit(() -> TreeNode.create(0, maxDepth)));

        for (int d = 4; d <= maxDepth; d += 2) {
            final int depth = d;
            final int iterations = 16 << (maxDepth - depth);

            list.add(executor.submit(() -> {
                int check = 0;
                for (int item = 0; item < iterations; item++) {
                    check += TreeNode.create(item, depth).check() + TreeNode.create(-item, depth).check();
                }
                return (iterations << 1) + "\t trees of depth " + depth + "\t check: " + check;
            }));
        }

        return list;
    }

    static class TreeNode {
        int item;
        TreeNode left, right;

        static TreeNode create(int item, int depth) {
            TreeNode[] queue = new TreeNode[(1 << depth) - 1];
            queue[0] = new TreeNode(item);

            int head = 0, nodes = 0;
            int target = queue.length - 1;
            while (nodes < target) {
                item <<= 1;
                TreeNode node = queue[head++];
                node.left = new TreeNode(item - 1);
                node.right = new TreeNode(item);
                queue[++nodes] = node.left;
                queue[++nodes] = node.right;
            }
            return queue[0];
        }

        TreeNode(int item) {
            this.item = item;
        }

        int check() {
            return left == null ? item : left.check() - right.check() + item;
        }
    }
}
